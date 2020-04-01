#' @import ggplot2
make_plots <- function(){
  ggplot(data[case_days>0], aes(x=case_days, y=log_cases, group=region))+geom_line()+theme_bw()
  ggplot(data[case10_days>0&max_cases>200], aes(x=case10_days, y=cases, group=region))+geom_line()+theme_bw()
  ggplot(data[case100_days>0&max_cases>200&country=="China"], aes(x=case100_days, y=log(cases), group=region))+geom_line()+theme_bw()
}