#include <cpp11.hpp>
#include <iostream>
using namespace cpp11;
namespace writable = cpp11::writable;

// Because our kernel is all 1s, we can use a rolling sum of the kernel size to perform the convolution.
// First we perform a rolling sum using the kernel size for each row (store as conv_matrix). O(i * (j + k))
// Then, for each column in conv_matrix, we can perform a rolling sum using the kernel size. O((i + k) * j)
// If any cell is >= threshold, return true.
//
// Complexity:
// i = image pixel rows;
// j = image pixel columns;
// k = kernel width / height
// k << i; k << j;
// i + j << i * j
// Total complexity is O(2 * i * j + k * (i + j)) which fully reduces to O(i * j);
// Typically this is O(2 * i * j + 5 * (i + j)) which is close to O(2 * i * j);
// Our method is better than O(i * j * k * k) which in practice is ~ O(25 * i * j) if k = 5;
// Generic method example: https://github.com/gjmvanboxtel/gsignal/blob/8f682a4036ccaaa8dfe95a2ae07cc5fb3be28863/src/conv2d.cpp#L25-L50
// Idea to separate the convolution matrix into two parts: https://towardsdatascience.com/a-basic-introduction-to-separable-convolutions-b99ec3102728
[[cpp11::register]] bool image_diff_breaks_threshold(cpp11::doubles_matrix<> diff_matrix, int kernel_size, double threshold)
{

  int diff_cols = diff_matrix.ncol();
  int diff_rows = diff_matrix.nrow();
  std::cout << "diff_cols: " << diff_cols << std::endl;
  std::cout << "diff_rows: " << diff_rows << std::endl;

  // Perform rolling sum for each row, given the diff_matrix
  // Store result into conv_matrix
  writable::doubles_matrix<> conv_matrix(diff_rows, diff_cols);
  for (int i = 0; i < diff_rows; ++i)
  {
    double row_rolling_sum = 0.0;
    // int i_pos = i - kernel_size;
    for (int j = 0; j < diff_cols + kernel_size - 1; ++j)
    {
      int j_prev = j - kernel_size;

      // Add to end of rolling sum
      if (j < diff_cols)
        row_rolling_sum += diff_matrix(i, j);

      if (j_prev >= 0) {
        // Remove from beginning of rolling sum
        row_rolling_sum -= diff_matrix(i, j_prev);
        // Since we know all values are non-negative, if we see a bad value early, quit early!
        // if (row_rolling_sum > 0) std::cout << "row_rolling_sum: " << row_rolling_sum << std::endl;
        if (row_rolling_sum > threshold) return true;
        // Store result
        conv_matrix(i, j_prev) = row_rolling_sum;
      }
    }
  }
  // Perform rolling sum for each column, given the conv_matrix
  for (int j = 0; j < conv_matrix.ncol(); ++j)
  {
    double col_rolling_sum = 0.0;
    for (int i = 0; i < conv_matrix.nrow() + kernel_size - 1; ++i)
    {
      int i_prev = i - kernel_size;

      // Add to end of rolling sum
      if (i < diff_rows)
        col_rolling_sum += conv_matrix(i, j);

      if (i_prev >= 0) {
        // Remove from beginning of rolling sum
        col_rolling_sum -= diff_matrix(i_prev, j);
        // Since we know all values are non-negative, if we see a bad value early, quit early!
        // if (col_rolling_sum > 0)
        //   std::cout << "col_rolling_sum: " << col_rolling_sum << std::endl;
        if (col_rolling_sum > threshold) return true;
        // No need to store result
        // out_matrix(i_prev, j) = col_rolling_sum;
      }
    }
  }

  // No bad threshold value found
  return false;
}
