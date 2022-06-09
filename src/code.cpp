#include "cpp11/matrix.hpp"
#include "cpp11/doubles.hpp"
// #include "Rmath.h"
#include <numeric>
#include <iostream>
#include <vector>
#include <functional>

#include <cpp11.hpp>
using namespace cpp11;
namespace writable = cpp11::writable;

[[cpp11::register]]
bool image_diff_breaks_threshold(cpp11::doubles_matrix<> diff_matrix, int kernal_size, double threshold) {


  int diff_cols = diff_matrix.ncol();
  int diff_rows = diff_matrix.nrow();

  int max_i = diff_rows + 2 * (kernal_size - 1);
  int max_j = diff_rows + 2 * (kernal_size - 1);
  writable::doubles_matrix<> conv_matrix(max_i, max_j);

  // cpp11::matrix_slices<by_row> diff_matrix_rows(diff_matrix);
  // cpp11::matrix_slices<by_column> diff_matrix_cols(diff_matrix);

  // writable::doubles_matrix<> rolled_sum(diff_rows, diff_cols);

  // for (cpp11::matrix<cpp11::r_vector<double>, double, cpp11::by_column>::slice diff_col_vals : diff_matrix)
  // {
  //   diff_col_vals.;
  // }

  // Because our kernal is all 1s, we can use a rolling sum of the kernal size to perform the convolution.
  // First we perform a rolling sum using the kernal size for each row (store as conv_matrix). O(i * (j + k))
  // Then, for each column in conv_matrix, we can perform a rolling sum using the kernal size. O((i + k) * j)
  // If any cell is >= threshold, return true.
  //
  // Complexity:
  // i = image pixel rows;
  // j = image pixel columns;
  // k = kernal width / height
  // k << i; k << j;
  // i + j << i * j
  // Total complexity is O(2 * i * j + k * (i + j)) which fully reduces to O(i * j);
  // Typically this is O(2 * i * j + 5 * (i + j)) which is close to O(2 * i * j);
  // Our method is better than O(i * j * k * k) which in practice is ~ O(25 * i * j) if k = 5;
  // Generic method example: https://github.com/gjmvanboxtel/gsignal/blob/8f682a4036ccaaa8dfe95a2ae07cc5fb3be28863/src/conv2d.cpp#L25-L50
  writable::doubles_matrix<> conv_matrix2(diff_rows, diff_cols);
  // Perform rolling sum for each row, given the diff_matrix
  // Store result into conv_matrix2
  for (int i = 0; i < diff_rows; ++i)
  {
    double row_rolling_sum = 0.0;
    // int i_pos = i - kernal_size;
    for (int j = 0; j < diff_cols + kernal_size - 1; ++j)
    {
      int j_prev = j - kernal_size;

      // Add to end of rolling sum
      if (j < diff_cols)
        row_rolling_sum += diff_matrix(i, j);

      if (j_prev >= 0) {
        // Remove from beginning of rolling sum
        row_rolling_sum -= diff_matrix(i, j_prev);
        // Since we know all values are non-negative, if we see a bad value early, quit early!
        if (row_rolling_sum > threshold) return true;
        // Store result
        conv_matrix2(i, j_prev) = row_rolling_sum;
      }
    }
  }
  // Perform rolling sum for each column, given the conv_matrix2
  for (int j = 0; j < conv_matrix2.ncol(); ++j)
  {
    double col_rolling_sum = 0.0;
    for (int i = 0; i < conv_matrix2.nrow() + kernal_size - 1; ++i) {
      int i_prev = i - kernal_size;

      // Add to end of rolling sum
      if (i < diff_rows)
        col_rolling_sum += conv_matrix2(i, j);

      if (i_prev >= 0) {
        // Remove from beginning of rolling sum
        col_rolling_sum -= diff_matrix(i_prev, j);
        // Since we know all values are non-negative, if we see a bad value early, quit early!
        if (col_rolling_sum > threshold) return true;
        // No need to store result
        // out_matrix(i_prev, j) = col_rolling_sum;
      }
    }
  }

  // No bad threshold value found
  return false;


  for (int i = 0; i < max_i; ++i) {
    double row_rolling_sum = 0.0;
    int i_pos = i - kernal_size;
    for (int j = 0; j < max_j; ++j) {
      int j_pos = j - kernal_size;
      int j_prev_pos = j_pos - kernal_size;

      if (
        j_pos >= 0 && i_pos >= 0 &&
        j_pos < diff_cols && i_pos < diff_rows
      ) {
        // Add to end of rolling sum
        row_rolling_sum += diff_matrix(i_pos, j_pos);
      }
      if (
        j_prev_pos >= 0 && i_pos >= 0 &&
        j_prev_pos < diff_cols && i_pos < diff_rows
      ) {
        // Remove beginning of rolling sum
        row_rolling_sum -= diff_matrix(i_pos, j_prev_pos);
      }
      // Since we know all values are non-negative, if we see a bad value early, quit early!
      if (row_rolling_sum > threshold) {
        return true;
      }
      conv_matrix(i, j) = row_rolling_sum;
    }
  }

  for (int j = 0; j < max_j; ++j) {
    double col_rolling_sum = 0.0;
    int j_pos = j - kernal_size;
    int j_prev_pos = j - kernal_size;
    for (int i = 0; i < max_i; ++i) {
      int i_pos = i - kernal_size;

      if (
        i_pos >= 0 && j_pos >= 0 &&
        i_pos < diff_rows && j_pos < diff_cols
      ) {
        col_rolling_sum += conv_matrix(i, j);
      }
      if (
        i_pos >= 0 && j_prev_pos >= 0 &&
        i_pos < diff_rows && j_prev_pos < diff_cols
      ) {
        col_rolling_sum -= conv_matrix(i, j);
      }
      // If any final cell is larger than threshold, quit!
      if (col_rolling_sum > threshold) {
        return true;
      }
      // Do not store result
    }
  }

  // No values above the threshold were found
  return false;

  // diff_matrix.slice_stride

  // double sum = 0.0;
  // for (int i = 0; i < diff_rows - kernal_size - 1; ++i) {
  //   for (int j = 0; j < diff_cols - kernal_size - 1; ++j)
  //   {
  //     conv_matrix(i, j) = diff_matrix(i, j);
  //     std::vector<double> temp_conv_matrix(kernal_size);

  //     for (int m = 0; m < kernal_size; m++) {
  //       temp_conv_matrix[m] = diff_matrix(i, j + m);
  //     }
  //     int im = i - kernal_size;
  //     for (int n = 0; n < kernal_size; n++)
  //     {
  //       int jn = j - n;
  //       if (im >= 0 && jn >= 0 && im < diff_rows && jn < diff_cols)
  //       {
  //         conv_matrix(i, j) += diff_matrix(im, jn) * 1;
  //       }
  //     }
  //   }
  // }

  // return sum <= threshold;
}
