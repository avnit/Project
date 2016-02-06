#include <Rcpp.h>
using namespace Rcpp;

// This is a simple function using Rcpp that creates an R list
// containing a character vector and a numeric vector.
//
// Learn more about how to use Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//
// and browse examples of code using Rcpp at:
//
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
List rcpp_getPrices(NumericVector Prices) {
  CharacterVector x = CharacterVector::create("Date", "price");
  NumericVector y   = NumericVector::create(0.0, 1.0);
  List z            = List::create(x, y);
  return z;
}
// [[Rcpp::export]]
Rcpp::DataFrame rcpp_ModifyDataFrame(DataFrame StockInput , DataFrame SugarPriceInput) {
  Rcpp::DataFrame stock = StockInput;
  Rcpp::DataFrame SugarPrice= SugarPriceInput;
  Rcpp::DataFrame Output;

  Rcpp::CharacterVector namevec;
  std::string namestem = "Column Heading ";


 std::vector<std::string> columnNamesStock = stock.attributeNames();
 std::vector<std::string> columnNamesSugar = SugarPrice.attributeNames();
 for (int i=0;i<stock.length();i++) {
   Output.push_back(stock(i));
   namevec.push_back(namestem+std::string(1,(char)(((int)'a') + i)));
 }
 Output.attr("names") = namevec;

 for(int j=0;j<SugarPrice.length();j++)
 {
   Output.push_back(SugarPrice(j));
   namevec.push_back(namestem+std::string(1,(char)(((int)'A') + j)));
 }

 return Output;
 //return Rcpp::DataFrame::create( Named("StockDate")= Output(1), Named("Close") = Output(5));
  }


