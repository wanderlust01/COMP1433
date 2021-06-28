sample_len <- function(v=26){
  data=1:v
  tmp_sample=0
  sample_cnt = 0
  while (tmp_sample!=1){
    tmp_sample=sample(x=data,size=1)
    sample_cnt=1+sample_cnt
  }
  sample_cnt
}

sample_lens <- function(n, v=26){
  result=c()
  for (k in 1:n){
    result = c(result, sample_len(v))
  }
  result
}

T=2000  # simulation numbers
lens=sample_lens(T)
print(mean(lens))  # E[X] is 26.
hist(lens)

n_vec=c()
ex_vec=c()
for (N in 2:26){
  lens=sample_lens(T, N)
  n_vec=c(n_vec, N)
  ex_vec=c(ex_vec, mean(lens))  # E[X] is N.
}
plot(n_vec, ex_vec)
