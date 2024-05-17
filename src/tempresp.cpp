// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <Rcpp.h>

// [[Rcpp::export(.tresp_Rcpp)]]
arma::mat tresp_Rcpp(arma::mat tmat, arma::vec tmin, arma::vec tmax, arma::vec topt) {
  
  // number of temperature values 
  int nv = tmat.n_rows;
  // number of species
  int ns = tmat.n_cols;
  
  arma::mat out(nv, ns, arma::fill::zeros);
  
  double term1, term2, term3, term4, term5;
  
  for (int i = 0; i < ns; i++) {
    
    term1 = tmax(i) - topt(i);
    term2 = topt(i) - tmin(i);
    term5 = term2 / term1;
    
    for (int j = 0; j < nv; j++) {
      
      term3 = tmax(i) - tmat(j, i);
      term4 = tmat(j, i) - tmin(i);
      
      if ((term3 > 0) & (term4 > 0)) {
        out(j, i) = (term3 / term1) * R_pow(term4 / term2, term5);
      }
      
    }
  }
  
  return out;
  
}
