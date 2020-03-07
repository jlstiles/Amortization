#' @export
M = function(r,S,P,period, tax,insurance, expenses) {
  if (period == 0) {
  r*P*exp(r*S)/(exp(r*S)-1)/12 + (tax+insurance+expenses)/12
  } else {
    d = seq(0,S,period)
    gain = P*(1+r*period)^(S/period)
    minus = sum(vapply(0:(length(d)-1), function(t) {
      (1+r*period)^t
    }, FUN.VALUE = 1))
    (gain/minus)/(12*period) + (tax+insurance+expenses)/12
    }
}


