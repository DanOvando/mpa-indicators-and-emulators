#include <Rcpp.h>
#include <queue>
#include <tuple>
#include <unordered_map>
using namespace Rcpp;
using namespace std;

// [[Rcpp::export]]
List select_contiguous_mpa(DataFrame df, int k) {
  IntegerVector x = df["x"];
  IntegerVector y = df["y"];
  NumericVector value = df["patch_weight"];
  
  if (x.size() != y.size() || x.size() != value.size()) {
    stop("x, y, and value must be the same length");
  }
  
  // Determine grid bounds
  int max_x = max(x), max_y = max(y);
  int rows = max_y, cols = max_x;
  
  // Create matrix of values
  NumericMatrix mat(rows, cols);
  LogicalMatrix mask(rows, cols);
  
  // Store coordinates to index mapping
  for (int i = 0; i < x.size(); ++i) {
    int row = rows - y[i];     // Convert Cartesian y to matrix row
    int col = x[i] - 1;        // 1-based x to 0-based col
    mat(row, col) = value[i];
    mask(row, col) = true;     // mark valid cell
  }
  
  typedef tuple<int, int, double> Cell;
  auto cmp = [](const Cell &a, const Cell &b) {
    return get<2>(a) < get<2>(b); // Max-heap
  };
  priority_queue<Cell, vector<Cell>, decltype(cmp)> frontier(cmp);
  vector<vector<bool>> visited(rows, vector<bool>(cols, false));
  vector<pair<int, int>> result;
  
  // Find max value cell among valid cells
  int sr = 0, sc = 0;
  double max_val = R_NegInf;
  for (int r = 0; r < rows; ++r) {
    for (int c = 0; c < cols; ++c) {
      if (mask(r, c) && mat(r, c) > max_val) {
        max_val = mat(r, c);
        sr = r;
        sc = c;
      }
    }
  }
  
  visited[sr][sc] = true;
  result.emplace_back(sr, sc);
  
  // Directions (4-connected)
  int dr[4] = {-1, 1, 0, 0}, dc[4] = {0, 0, -1, 1};
  
  // Add neighbors
  for (int d = 0; d < 4; ++d) {
    int nr = sr + dr[d], nc = sc + dc[d];
    if (nr >= 0 && nr < rows && nc >= 0 && nc < cols &&
        mask(nr, nc) && !visited[nr][nc]) {
      frontier.emplace(nr, nc, mat(nr, nc));
    }
  }
  
  // BFS greedy expansion
  while (result.size() < (size_t)k && !frontier.empty()) {
    auto [r, c, val] = frontier.top();
    frontier.pop();
    if (visited[r][c]) continue;
    visited[r][c] = true;
    result.emplace_back(r, c);
    
    for (int d = 0; d < 4; ++d) {
      int nr = r + dr[d], nc = c + dc[d];
      if (nr >= 0 && nr < rows && nc >= 0 && nc < cols &&
          mask(nr, nc) && !visited[nr][nc]) {
        frontier.emplace(nr, nc, mat(nr, nc));
      }
    }
  }
  
  // Convert back to x/y coordinates
  IntegerVector sel_x(result.size()), sel_y(result.size());
  double total = 0;
  
  for (size_t i = 0; i < result.size(); ++i) {
    int r = result[i].first;
    int c = result[i].second;
    sel_x[i] = c + 1;
    sel_y[i] = rows - r;
    total += mat(r, c);
  }
  
  DataFrame coords = DataFrame::create(
    Named("x") = sel_x,
    Named("y") = sel_y
  );
  
  return List::create(
    Named("coordinates") = coords,
    Named("total_value") = total
  );
}
