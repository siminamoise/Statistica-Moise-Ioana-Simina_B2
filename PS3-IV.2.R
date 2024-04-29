binomial_probability = function(n, k, t) 
{
  expectation = n*k;
  variance = n*k*(1 - k);
  standard_deviation = sqrt(variance);
  q = (t + 0.5)/standard_deviation;
  return(1 - pnorm(q));
}
binomial_probability(50, 0.3, 7)
