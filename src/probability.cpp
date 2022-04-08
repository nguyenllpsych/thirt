#include <Rcpp.h>
using namespace Rcpp;
using namespace std;

NumericVector p_thirt_oneC(NumericVector gamma,
                           NumericVector lambda1, NumericVector lambda2,
                           NumericVector theta1, NumericVector theta2,
                           NumericVector psisq1, NumericVector psisq2) {

  // create empty vectors of correct length
  NumericVector num(theta1.length()),
  result(theta1.length());

  double den = sqrt(pow(psisq1[0], 2) + pow(psisq2[0], 2));

  // populate vectors with values
  for (int i = 0; i < theta1.length(); i++) {
    num[i] = -gamma[0] + lambda1[0] * theta1[i] - lambda2[0] * theta2[i];
    result[i] = R::pnorm(num[i]/den, 0.0, 1.0, 1, 0);
  }

  return result;
}

int split_pairC(string pair,
                int item) {

  // delimiter "-" separates 2 items
  string delim = "-";
  int delim_idx = pair.find(delim);
  int pair_len = pair.length();

  // 1st item starts at 0 and 2nd item starts after delim index
  int item_start = (item == 1) ? 0 : delim_idx + 1;

  // 1st item has length of delim index
  // 2nd item has length of (pair length - delim index - 1)
  int item_len = (item == 1) ? delim_idx : pair_len - delim_idx - 1;

  // extract item as string
  string item_string = pair.substr(item_start, item_len);

  // convert string to int
  int item_num = stoi(item_string);
  return item_num;
}

String join_pairC(CharacterVector perm_id,
                  IntegerMatrix perm_item,
                  String resp,
                  IntegerVector col) {

  CharacterVector resp_vector(perm_id.length(), resp);
  LogicalVector id_idx = (perm_id == resp_vector);
  IntegerVector col_1 = perm_item(_, col[0] - 1);
  IntegerVector col_2 = perm_item(_, col[1] - 1);

  IntegerVector perm_item_1 = col_1[id_idx];
  IntegerVector perm_item_2 = col_2[id_idx];

  String joined_pair = to_string(perm_item_1[0]) + "-" + to_string(perm_item_2[0]);
  return joined_pair;
}

// [[Rcpp::export]]
NumericMatrix p_thirt_blockC(int n_person,
                             int n_dim,
                             int block,
                             int n_perm,
                             int n_item,
                             CharacterVector pair_names,
                             CharacterVector perm_id,
                             IntegerVector perm_item_vector,
                             NumericVector params_gamma,
                             NumericVector params_lambda,
                             NumericVector params_psisq,
                             NumericVector vector_theta,
                             IntegerVector dict_item,
                             IntegerVector dict_dim,
                             IntegerVector picked_order) {

  /* picked_order is a vector of length n_person with the response id
   * following mupp::find_all_permutation orders
   */

  /* EXTRACT IMPORTANT INFO
   * some matrices need to be built from vectors
   */

  NumericMatrix params_theta(n_person, n_dim, vector_theta.begin());
  IntegerMatrix perm_item(n_perm, n_item, perm_item_vector.begin());

    /* PAIRWISE PROBABILITY
   * prob_pair is a NumericMatrix of dim [n_person X (block_size * 2)]
   *    of probabilities of choosing each item in all item pairs in a block
   */

  NumericMatrix prob_pair(n_person, pair_names.length() * 2);
  CharacterVector prob_names(pair_names.length() * 2);

  for(int i = 0; i < pair_names.length(); i++) {

    // pair names
    String pair_name = pair_names[i];
    int pair1        = split_pairC(pair_name.get_cstring(), 1);
    int pair2        = split_pairC(pair_name.get_cstring(), 2);

    // index everything
    LogicalVector gamma_idx(params_gamma.length()),
                  item1_idx(params_lambda.length()),
                  item2_idx(params_lambda.length()),
                  psisq1_idx(params_psisq.length()),
                  psisq2_idx(params_psisq.length()),
                  dim1_idx(dict_dim.length()),
                  dim2_idx(dict_dim.length());
    for(int i = 0; i < gamma_idx.length(); i++) {
      gamma_idx[i] = (pair_names[i] == pair_name.get_cstring());
    }
    for(int i = 0; i < item1_idx.length(); i++) {
      item1_idx[i] = (dict_item[i] == pair1);
      item2_idx[i] = (dict_item[i] == pair2);
      dim1_idx[i] = (dict_item[i] == pair1);
      dim2_idx[i] = (dict_item[i] == pair2);
    }

    IntegerVector theta1_idx = dict_dim[dim1_idx];
    IntegerVector theta2_idx = dict_dim[dim2_idx];

    NumericVector gamma_p = params_gamma[gamma_idx];
    NumericVector lambda1_p = params_lambda[item1_idx];
    NumericVector lambda2_p = params_lambda[item2_idx];
    NumericVector psisq1_p = params_psisq[item1_idx];
    NumericVector psisq2_p = params_psisq[item2_idx];
    NumericVector theta1_p = params_theta(_, theta1_idx[0] - 1);
    NumericVector theta2_p = params_theta(_, theta2_idx[0] - 1);

    // p_thirt_one calculations
    NumericVector current_prob = p_thirt_oneC(gamma_p, lambda1_p, lambda2_p,
                                              theta1_p, theta2_p, psisq1_p, psisq2_p);

    // add probabilities of reverse response patterns
    NumericVector current_rprob = 1 - current_prob;

    // add to prob_pair matrix
    int prob_idx = i * 2;
    int rprob_idx = prob_idx + 1;
    prob_pair( _, prob_idx) = current_prob;
    prob_pair( _, rprob_idx) = current_rprob;

    // names
    string rpair_name = to_string(pair2) + "-" + to_string(pair1);
    prob_names[prob_idx] = pair_name;
    prob_names[rprob_idx] = rpair_name;
  }
  colnames(prob_pair) = prob_names;

  /* RANKED SEQUENCE PROBABILITY
   * probability is a NumericMatrix of dim [n_person X n_permutation]
   *    of probabilities of different response patterns per block
   */

  /* for ALL permutations
   * if no picked_order provided
   */
  if(picked_order[0] == NA_INTEGER) {

    NumericMatrix probability(n_person, n_perm);

    // iterate through response patterns per block
    for(int resp = 0; resp < n_perm; resp++) {

      NumericVector probability_col(n_person, 1.0);

      // iterate through number of separate probabilities to joint multiply
      // moving from the end/smaller probability first
      // e.g. 2 master probabilities for 3 items: p(1 > 2, 3) * p(2 > 3)
      for(int probmaster = 1; probmaster < n_item; probmaster++) {

        CharacterVector num_list(probmaster);
        // iterate through number of probabilities in the numerator
        // num_list is a vector of pairwise probability names
        // e.g. for master probability p(1 > 2, 3):
        //      numerator = p(1 > 2) * p(1 > 3)
        //      num_list  = c("1-2", "1-3")

        IntegerVector item_list;

        for(int numlen = 1; numlen < probmaster + 1; numlen++) {
          IntegerVector col = {n_item - probmaster, n_item - (probmaster - 1 * numlen)};
          num_list[numlen - 1] = join_pairC(perm_id, perm_item, perm_id[resp], col);
          String item_pair = num_list[numlen - 1];
          item_list.push_back(split_pairC(item_pair.get_cstring(), 1));
          item_list.push_back(split_pairC(item_pair.get_cstring(), 2));
        }

        item_list = unique(item_list);

        // create a matrix of all denominators that correspond to a numerator
        // each den_vector is a vector of denominators
        //   e.g. probmaster   = p(1 > 2, 3)
        //        numerator    = p(1 > 2) * p(1 > 3)
        //        denominator  = [p(1 > 2) * p(1 > 3)]
        //                       + [p(2 > 1) * p(2 > 3)]
        //                       + [p(3 > 1) * p(3 > 2)]
        //        den_vectors  = c("1-2", "1-3"),
        //                       c("2-1", "2-3"),
        //                       c("3-1", "3-2")
        //
        // probability calculation: (1) multiply elements within each column in den_list
        //                          (2) sum across all vectors within den_list
        //                          (3) multiply elements in num_list
        //                          (4) divide (3) by (2)
        //                          (5) multiply (4) across all probmaster

        NumericVector step2(n_person, 0.0);

        for(int item = 0; item < item_list.length(); item++) {

          IntegerVector first_item = {item_list[item]};
          IntegerVector second_item = setdiff(item_list, first_item);

          CharacterVector den_vector(item_list.length() - 1);

          // step 1: multiply all probabilities for elements within a den_vector in den_list
          //         e.g. p(1 > 2) * p(1 > 3) and p(2 > 1) * p(2 > 3) and ...

          NumericVector step1(n_person, 1.0);

          for(int den_col = 0; den_col < den_vector.length(); den_col++) {
            den_vector[den_col] = to_string(first_item[0]) + "-" + to_string(second_item[den_col]);

            // select column from prob_pair that matches den_vector[den_col]
            int prob_pair_idx;
            for(int name = 0; name < prob_names.length(); name++) {
              if(prob_names[name] == den_vector[den_col]) {
                prob_pair_idx = name;
              }
            }

            NumericVector prob_pair_col = prob_pair(_, prob_pair_idx);

            step1 = step1 * prob_pair_col;
          }

          // step 2: sum across all den_vector
          //         e.g. [p(1 > 2) * p(1 > 3)] + [p(2 > 1) * p(2 > 3)] + ...

          step2 = step2 + step1;
        }

        // step 3: multiply elements in num_list
        //         e.g. p(1 > 2) * p(1 > 3)

        NumericVector step3(n_person, 1.0);

        for(int num_col = 0; num_col < num_list.length(); num_col++) {

          // select column from prob_pair that matches num_list[num_col]
          int prob_pair_idx2;
          for(int name2 = 0; name2 < prob_names.length(); name2++) {
            if(prob_names[name2] == num_list[num_col]) {
              prob_pair_idx2 = name2;
            }
          }

          NumericVector prob_pair_col2 = prob_pair(_, prob_pair_idx2);

          step3 = step3 * prob_pair_col2;
        }

        // step 4: divide step 3 by step 2

        NumericVector step4 = step3 / step2;

        // step 5: multiply step 4 across all probmaster and populate probability matrix
        //         e.g. p(1 > 2, 3) * p(2 > 3)
        probability_col = probability_col * step4;
      }

      probability( _ , resp) = probability_col;
    }
    return probability;
  }

  /* for ONLY selected resp
   * if picked_order provided
   */

  if(picked_order[0] != NA_INTEGER) {

    // probability is a matrix of dimension [person x 1]
    NumericMatrix probability(n_person, 1);

    // picked_order for current block
    // vector of length n_person
    CharacterVector resp(picked_order.length());
    for(int p = 0; p < resp.length(); p++) {
      resp[p] = "b" + to_string(block) + "r" + to_string(picked_order[p]);
    }

    // iterate through responses for each person
    for(int r = 0; r < resp.length(); r++) {

      // probability col is a double for each person's probability
      double probability_col = 1.0;

      // iterate through number of separate probabilities to joint multiply
      // moving from the end/smaller probability first
      // e.g. 2 master probabilities for 3 items: p(1 > 2, 3) * p(2 > 3)
      for(int probmaster = 1; probmaster < n_item; probmaster++) {

        // iterate through number of probabilities in the numerator
        // num_list is a vector of pairwise probability names
        // e.g. for master probability p(1 > 2, 3):
        //      numerator = p(1 > 2) * p(1 > 3)
        //      num_list  = c("1-2", "1-3")

        CharacterVector num_list(probmaster);
        IntegerVector item_list;

        for(int numlen = 1; numlen < probmaster + 1; numlen++) {

          // columns of items to join pairs
          IntegerVector col = {n_item - probmaster, n_item - (probmaster - 1 * numlen)};

          num_list[numlen - 1] = join_pairC(perm_id, perm_item, resp[r], col);

          // create a list of items present in the numerators
          // each column is one person
          String item_pair = num_list[numlen - 1];
          item_list.push_back(split_pairC(item_pair.get_cstring(), 1));
          item_list.push_back(split_pairC(item_pair.get_cstring(), 2));
        }
        item_list = unique(item_list);

        // create a matrix of all denominators that correspond to a numerator
        // each den_vector is a vector of denominators
        //   e.g. probmaster   = p(1 > 2, 3)
        //        numerator    = p(1 > 2) * p(1 > 3)
        //        denominator  = [p(1 > 2) * p(1 > 3)]
        //                       + [p(2 > 1) * p(2 > 3)]
        //                       + [p(3 > 1) * p(3 > 2)]
        //        den_vectors  = c("1-2", "1-3"),
        //                       c("2-1", "2-3"),
        //                       c("3-1", "3-2")
        //
        // probability calculation: (1) multiply elements within each column in den_list
        //                          (2) sum across all vectors within den_list
        //                          (3) multiply elements in num_list
        //                          (4) divide (3) by (2)
        //                          (5) multiply (4) across all probmaster

        double step2 = 0.0;

        for(int item = 0; item < item_list.length(); item++) {

          IntegerVector first_item = {item_list[item]};
          IntegerVector second_item = setdiff(item_list, first_item);
          CharacterVector den_vector(item_list.length() - 1);

          // step 1: multiply all probabilities for elements within a den_vector in den_list
          //         e.g. p(1 > 2) * p(1 > 3) and p(2 > 1) * p(2 > 3) and ...
          double step1 = 1.0;
          for(int den_col = 0; den_col < den_vector.length(); den_col++) {
            den_vector[den_col] = to_string(first_item[0]) + "-" + to_string(second_item[den_col]);

            // select column from prob_pair that matches den_vector[den_col]
            int prob_pair_idx;
            for(int name = 0; name < prob_names.length(); name++) {
              if(prob_names[name] == den_vector[den_col]) {
                prob_pair_idx = name;
              }
            }
            double prob_pair_col = prob_pair(r, prob_pair_idx);
            step1 = step1 * prob_pair_col;
          }
          // step 2: sum across all den_vector
          //         e.g. [p(1 > 2) * p(1 > 3)] + [p(2 > 1) * p(2 > 3)] + ...
          step2 = step2 + step1;
        }

        // step 3: multiply elements in num_list
        //         e.g. p(1 > 2) * p(1 > 3)

        double step3 = 1.0;

        for(int num_col = 0; num_col < num_list.length(); num_col++) {

          // select column from prob_pair that matches num_list[num_col]
          int prob_pair_idx2;
          for(int name2 = 0; name2 < prob_names.length(); name2++) {
            if(prob_names[name2] == num_list[num_col]) {
              prob_pair_idx2 = name2;
            }
          }

          double prob_pair_col2 = prob_pair(r, prob_pair_idx2);

          step3 = step3 * prob_pair_col2;
        }

        // step 4: divide step 3 by step 2

        double step4 = step3 / step2;

        // step 5: multiply step 4 across all probmaster and populate probability matrix
        //         e.g. p(1 > 2, 3) * p(2 > 3)
        probability_col = probability_col * step4;
      }

      probability(r, 0) = probability_col;
    }
  return probability;
  }
  return 0;
}
