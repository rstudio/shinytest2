#include <cpp11.hpp>
using namespace cpp11;

// Convolution matrix calculation where the kernel is K x K in size and full of 1s.
// Also, the incoming matrix is non-negative. Meaning that any rolling sum values found will never decrease.
// This gives the opportunity to return early if the maximum value is found.
// However, the max value will always be returned as resolving errors is more
// important than speed
//
// Generic method example: https://github.com/gjmvanboxtel/gsignal/blob/8f682a4036ccaaa8dfe95a2ae07cc5fb3be28863/src/conv2d.cpp#L25-L50
// Idea to separate the convolution matrix into two parts: https://towardsdatascience.com/a-basic-introduction-to-separable-convolutions-b99ec3102728
//
// Thought process:
// Because our kernel is all `1`s, we can use a rolling sum of the kernel size to perform the convolution.
// First we perform a rolling sum using the kernel size for each row (store as conv_matrix). O(i * (j + k))
// Then, for each column in conv_matrix, we can perform a rolling sum using the kernel size. O((i + k) * j)
//
// Ideally, we should return the max value found to inform the user of how different the images are.
//  By returning the number, we can give more information such as
// "`X` was larger than the value `threshold` that you supplied... images are not the same."
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
//
// Return value:
//   Return max convolution value
[[cpp11::register]] double image_diff_convolution_max_value(
    cpp11::doubles_matrix<by_column> diff_matrix,
    int kernel_size)
{
  double max_value = 0.0;

  int diff_cols = diff_matrix.ncol();
  int diff_rows = diff_matrix.nrow();

  // Perform rolling sum for each row, given the diff_matrix
  // Store result into conv_matrix
  cpp11::writable::doubles_matrix<by_column>
      conv_matrix(diff_rows, diff_cols);
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

        // Store result
        conv_matrix(i, j_prev) = row_rolling_sum;
      }
    }
  }

  // Perform rolling sum for each column, given the conv_matrix
  for (int j = 0; j < diff_cols; ++j)
  {
    double col_rolling_sum = 0.0;
    for (int i = 0; i < diff_rows + kernel_size - 1; ++i)
    {
      int i_prev = i - kernel_size;

      // Add to end of rolling sum
      if (i < diff_rows)
        col_rolling_sum += conv_matrix(i, j);

      if (i_prev >= 0) {
        // Remove from beginning of rolling sum
        col_rolling_sum -= conv_matrix(i_prev, j);

        // No need to store result
        // out_matrix(i_prev, j) = col_rolling_sum;

        // Update the max value
        if (max_value < col_rolling_sum)
          max_value = col_rolling_sum;
      }
    }
  }

  return max_value;
}