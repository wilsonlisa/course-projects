// longitudinal Rasch model for 2 time points (NEPS sample)
functions {
    real corr_mat_pdf_log(matrix R, real k) {
        real log_dens;
        log_dens = ((k * (k - 1) / 2) - 1) * log_determinant(R) + (-((k + 1) / 2)) * sum(log(diagonal(R)));
        return log_dens;
    }
}
data {
    int<lower=1> I; // number of persons
    int<lower=1> T; // number of time points
    int<lower=1> J[T]; // number of items per time point
    int<lower=0, upper=1> Y[I, sum(J)]; // binary item response data
}
parameters {
    matrix[I, T] theta; // latent ability
    vector[sum(J)] beta; // item difficulty
    real mu; // prior mean of latent ability (time point 2)
    corr_matrix[T] R; // correlation matrix of latent ability
    real<lower=0> SD; // std. deviation of latent ability (time point 2)
}
transformed parameters {
    vector[T] mutheta;
    vector[T] S;
    cov_matrix[T] sigmatheta;
    vector[sum(J)] BETA; // item difficulty

    mutheta[1] = 0;
    mutheta[2] = mu;
    S[1] = 1;
    S[2] = SD;
    sigmatheta = diag_matrix(S) * R * diag_matrix(S);
    BETA = beta;
    //BETA[7] = (beta[7]+beta[J[1]+5])/2;
    //BETA[J[1]+5] = BETA[7];
    //BETA[9] = (beta[9]+beta[J[1]+7])/2;
    //BETA[J[1]+7] = BETA[9];
    //BETA[10] = (beta[10]+beta[J[1]+8])/2;
    //BETA[J[1]+8] = BETA[10];
    //BETA[17] = (beta[17]+beta[J[1]+18])/2;
    //BETA[J[1]+18] = BETA[17];
    //BETA[18] = (beta[18]+beta[J[1]+20])/2;
    //BETA[J[1]+20] = BETA[18];
    //BETA[20] = (beta[20]+beta[J[1]+22])/2;
    //BETA[J[1]+22] = BETA[20];
}
model {
    mu ~ normal(1, 3);
    R ~ corr_mat_pdf(T);
    SD ~ normal(1, 3) T[0, ];

    for (i in 1:I) {
        theta[i, ] ~ multi_normal(mutheta, sigmatheta);
    }
    beta ~ normal(0, 3);

    for (j in 1:(sum(J))) {
        if (j > J[1]) {
            Y[, j] ~ bernoulli_logit(theta[, 1] + theta[, 2] - BETA[j]);
        } else {
            Y[, j] ~ bernoulli_logit(theta[, 1] - BETA[j]);
        }
    }
}
generated quantities {
    int y_rep[I, sum(J)];
    real log_lik[I, sum(J)];

    for (i in 1:I) {
        for (j in 1:sum(J)) {
            if (j > J[1]) {
                y_rep[i, j] = bernoulli_logit_rng(theta[i, 1] + theta[i, 2] - BETA[j]);
            } else {
                y_rep[i, j] = bernoulli_logit_rng(theta[i, 1] - BETA[j]);}}}
    for (i in 1:I) {
        for (j in 1:sum(J)) {
            if (j > J[1]) {
                log_lik[i, j] = bernoulli_logit_lpmf(Y[i, j] | theta[i, 1] + theta[i, 2] - BETA[j]);
           } else {
               log_lik[i, j] = bernoulli_logit_lpmf(Y[i, j] | theta[i, 1] - BETA[j]);
           }}}}
