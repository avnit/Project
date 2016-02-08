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
SEPX ElvisOperator (SEPX RuleName , SEPX ifTrue , SEPX ifFalse) {
  return  RuleName ? ifTrue : ifFalse;
}


// [[Rcpp::export]]
List CheapDataFrameBuilder(List a, SEXP Name, StringVector rowNames , StringVector ColumnNames , StringVector SugarNames , int counter) {
  List returned_frame = clone(a);
  GenericVector sample_row = returned_frame(0);

  StringVector row_names(sample_row.length());
  for (int i = 0; i < sample_row.length(); ++i) {
    char name[5];
    sprintf(&(name[0]), "%s.%d",CHAR(STRING_ELT(Name,0)), i);
    row_names(i) = name;
  }
  returned_frame.attr("row.names") = rowNames;

  StringVector col_names(returned_frame.length());
  for (int j = 0; j < returned_frame.length(); ++j) {
    if (j < counter)
    {
    col_names(j) = ColumnNames[j];
    }
    else
    {
      col_names(j) = SugarNames[j-counter];
    }
  }
  returned_frame.attr("names") = col_names;
  returned_frame.attr("class") = "data.frame";

  return returned_frame;
}

// [[Rcpp::export]]
Rcpp::DataFrame rcpp_ModifyDataFrame(DataFrame StockInput, DataFrame SugarPriceInput , SEXP Name) {
  Rcpp::DataFrame stock = StockInput;
  Rcpp::DataFrame SugarPrice= SugarPriceInput;
  Rcpp::DataFrame Output;

  Rcpp::CharacterVector namevec;
  std::string namestem = "name";


  StringVector RowNamesStock = stock.attr("row.names");
  StringVector ColumnNamesStock = stock.attr("names");
  StringVector SugarColumns = SugarPrice.attr("names");
 //std::vector<std::string> columnNamesSugar = SugarPrice.attributeNames();
 int z = 0;
 for (int i=0;i<stock.length();i++) {
   Output.push_back(stock(i));
   z++;
  // namevec.push_back(namestem+std::string(1,(char)(((int)'a') + i)));
 }
 Output.attr("names") = namevec;

 for(int j=0;j<SugarPrice.length();j++)
 {
    Output.push_back(SugarPrice(j));
 }
  // namevec.push_back(namestem+std::string(1,(char)(((int)'A') + j)));

  List ReturnedList = CheapDataFrameBuilder(Output,Name,RowNamesStock,ColumnNamesStock,SugarColumns,z);
 return ReturnedList;
 //return Rcpp::DataFrame::create( Named("StockDate")= Output(1), Named("Close") = Output(5));
  }



