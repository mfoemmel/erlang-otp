%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%% 
%%     $Id$
%%
%%%*********************************************************************
%%%
%%%   Description:      Library functions supposed to be used by the 
%%%                     application programmer. The functions are divided
%%%                     in two categories, one concerning statistics and 
%%%                     one concerning timing and synchronisation.
%%%
%%%*********************************************************************

-module(mesh_lib).



-export([sum/1,
	 sum_and_squaresum/1,
	 sample_mean/1,
	 sample_variance/1,
	 sample_mean_and_variance/1,
	 mean_variance/1,
	 ewma_mean/4,
	 ewma_variance/5,
	 uwma_mean/4,
	 uwma_variance/5
	]).




%%%*********************************************************************
%%%  EXPORTED FUNCTIONS
%%%*********************************************************************


%%======================================================================
%% Function:      sum
%%
%% Return Value:  SampleSum  |
%%                {error, Reason}
%%
%% Description:   Sums a list of samples.
%%
%% Parameters:    L  -- List of samples (integers or floats).
%%======================================================================

sum(L) ->
    lists:sum(L).



%%======================================================================
%% Function:      sum_and_squaresum
%%
%% Return Value:  {SampleSum, SampleSquareSum}  |  
%%                {error, Reason}
%%
%% Description:   Sums a list of samples, and also sums the square of all samples.
%%
%% Parameters:    L  -- List of samples (integers or floats).
%%======================================================================

sum_and_squaresum([]) ->
    {error, undefined_operation};
sum_and_squaresum(L) when list(L) ->
    sum_and_squaresum(L, 0, 0);
sum_and_squaresum(L) ->
    {error, {not_a_list, L}}.


sum_and_squaresum([H | T], Sum, SqSum) ->
    sum_and_squaresum(T, Sum+H, SqSum+(H*H));
sum_and_squaresum([], Sum, SqSum) ->
    {Sum, SqSum}.



%%======================================================================
%% Function:      sample_mean
%%
%% Return Value:  EstimatedSampleMean  |  
%%                {error, Reason}
%%
%% Description:   Suppose that [X1, X2, X3, ..., Xn] are random variables 
%%                (observations) with finite population mean u.
%%                The sample mean estimation m(n) = (X1 + X2 + X3 + ... +Xn) / n
%%
%% Parameters:    L  -- List of samples (integers or floats).
%%======================================================================

sample_mean([]) ->
    {error, undefined_operation};
sample_mean(L) when list(L) ->
    {Sum, N} = sum_and_length(L, 0, 0),
    Sum / N;
sample_mean(L) ->
    {error, {not_a_list, L}}.



%%======================================================================
%% Function:      sample_variance
%%
%% Return Value:  EstimatedSampleVariance  |  
%%                {error, Reason}
%%
%% Description:   Suppose that [X1, X2, X3, ..., Xn] are random variables 
%%                (observations) with finite population mean u and finite 
%%                population variance V, and sample mean estimate m(n).
%%                The sample variance estimation 
%%                S^2(n) = ((X1 - m(n))^2 + (X2 - m(n))^2 + ... + (Xn - m(n))^2) / (n - 1) =
%%                       = (X1^2 + X2^2 + ... + Xn^2 - (n * m(n)^2)) / (n - 1)
%%
%% Parameters:    L  -- List of samples (integers or floats).
%%======================================================================

sample_variance([]) ->
    {error, undefined_operation};
sample_variance([H]) ->
    {error, undefined_operation};
sample_variance(L) when list(L) ->
    {Sum, SqSum, N} = sum_squaresum_and_length(L, 0, 0, 0),
    Mean = Sum / N,
    (SqSum - (N*Mean*Mean)) / (N - 1);    
sample_variance(L) ->
    {error, {not_a_list, L}}.



%%======================================================================
%% Function:      sample_mean_and_variance
%%
%% Return Value:  {EstimatedSampleMean, EstimatedSampleVariance}  |  
%%                {error, Reason}
%%
%% Description:   Suppose that [X1, X2, X3, ..., Xn] are random variables 
%%                (observations) with finite population mean u and finite 
%%                population variance V, and sample mean estimate m(n).
%%                The sample mean estimation m(n) = (X1 + X2 + X3 + ... +Xn) / n
%%                The sample variance estimation 
%%                S^2(n) = ((X1 - m(n))^2 + (X2 - m(n))^2 + ... + (Xn - m(n))^2) / (n - 1) =
%%                       = (X1^2 + X2^2 + ... + Xn^2 - (n * m(n)^2)) / (n - 1)
%%
%% Parameters:    L  -- List of samples (integers or floats).
%%======================================================================

sample_mean_and_variance([]) ->
    {error, undefined_operation};
sample_mean_and_variance([H]) ->
    {error, undefined_operation};
sample_mean_and_variance(L) when list(L) ->
    {Sum, SqSum, N} = sum_squaresum_and_length(L, 0, 0, 0),
    Mean = Sum / N,
    {Mean, (SqSum - (N*Mean*Mean)) / (N - 1)};
sample_mean_and_variance(L) ->
    {error, {not_a_list, L}}.



%%======================================================================
%% Function:      mean_variance
%%
%% Return Value:  MeanVariance  |  
%%                {error, Reason}
%%
%% Description:   Suppose that [X1, X2, X3, ..., Xn] are INDEPENDENT random 
%%                variables with finite population mean u and finite 
%%                population variance V, and sample mean estimate m(n).
%%                The variance of the estimated mean itself may be estimated
%%                through the formula
%%                Var(m(n)) = S^2(n) / n
%%                NOTE: This formula is valid if and only if all Xi's are 
%%                independent (uncorrelated)!!! This is normally not the case 
%%                in, for example, simulations!
%%
%% Parameters:    L  -- List of estimated means.
%%======================================================================

mean_variance([]) ->
    {error, undefined_operation};
mean_variance([H]) ->
    {error, undefined_operation};
mean_variance(L) when list(L) ->
    {Sum, SqSum, N} = sum_squaresum_and_length(L, 0, 0, 0),
    Mean = Sum / N,
    (SqSum - (N*Mean*Mean)) / (N*(N-1));    
mean_variance(L) ->
    {error, {not_a_list, L}}.


%%======================================================================
%% Function:      ewma_mean
%%
%% Return Value:  EstimatedMean  |  
%%                {error, Reason}
%%
%% Description:   This function computes the mean of a number of samples 
%%                using the Exponentially Weighted Moving Average technique. 
%%                Suppose that [X1, X2, X3, ..., Xn] are random variables 
%%                (observations) with finite population mean u.
%%                Also assume we have previously computed a mean value estimate Wn
%%                (where W0 may have been simply estimated).
%%                Let GP denote the granularity period, i.e., the time elapsed
%%                between any two successive sample measurements, and let MTP
%%                denote the moving time period, i.e., the time within which 
%%                samples are considered. (For example, let GP be 5 ms, and 
%%                MTP 1 s, which means that the EWMA mean will be based on 
%%                200 samples.)
%%                When we receive a new sample Xnew, the new estimate of the 
%%                mean will be 
%%                Wnew = f * Xnew + (1 - f) * Wn, where f = 2 * GP / (GP + MTP)
%%
%% Parameters:    Xnew  -- The most recent sample.
%%                Wn    -- The previous mean estimate.
%%                GP    -- The granularity period.
%%                MTP   -- The moving time period.
%%======================================================================

ewma_mean(Xnew, Wn, GP, MTP) ->
    F = 2 * GP / (GP + MTP),
    F * Xnew + (1 - F) * Wn.


%%======================================================================
%% Function:      ewma_variance
%%
%% Return Value:  EstimatedVariance  |  
%%                {error, Reason}
%%
%% Description:   This function computes the variance of a number of samples 
%%                using the Exponentially Weighted Moving Average technique. 
%%                Suppose that [X1, X2, X3, ..., Xn] are random variables 
%%                (observations) with finite population mean u.
%%                Also assume we have previously computed a variance value 
%%                estimate Sn (where S0 may have been simply estimated).
%%                Let GP denote the granularity period, i.e., the time elapsed
%%                between any two successive sample measurements.
%%                Also, let SMTP denote the second moving time period, i.e.,
%%                the effective time interval over which values are scanned to 
%%                calculate an estimate of the variance.
%%                When we receive a new sample Xnew, the new estimate of the 
%%                mean will be 
%%                Snew = g * (Xnew - Wnew)^2 +  (1 - g) * Sn, where g = 2 * GP / (GP + SMTP)
%%                NOTE: the bias can be shown to be 
%%                u = 2*(1-f)^2/(2-f), times the variance of Xnew - Wnew (where f is taken
%%                from the ewma_mean formula). This may be used to reduce the bias in 
%%                the calculations, using the formula S'n = Sn / u.
%%                The manager may decide whether to reduce the bias or ignore it...
%%
%% Parameters:    Xnew  -- The most recent sample.
%%                Wnew  -- The current EWMA mean estimate.
%%                Sn    -- The previous variance estimate.
%%                GP    -- The granularity period.
%%                SMTP  -- The second moving time period.
%%======================================================================

ewma_variance(Xnew, Wnew, Sn, GP, SMTP) ->
    G = 2 * GP / (GP + SMTP),
    G * math:pow(Xnew - Wnew, 2) + (1 - G) * Sn.



%%======================================================================
%% Function:      uwma_mean
%%
%% Return Value:  {EstimatedMean, SampleSum}  |  
%%                {error, Reason}
%%
%% Description:   This function computes the variance of a number of samples 
%%                using the Uniformaly Weighted Moving Average technique. 
%%                Suppose that [X1, X2, X3, ..., Xn] are random variables 
%%                (observations) with finite population mean u, and sample
%%                sum SX.
%%                When we receive a new sample Xnew, the new estimate of the 
%%                mean will be 
%%                Wnew = Xnew + (SX - Xold) / N, where Xold is the oldest sample 
%%                used in the calculation of SX (i.e., the sample that will be 
%%                replaced by Xnew when calculating Wnew), and N is the number of 
%%                samples the calculation of Wnew is based on.
%%
%% Parameters:    Xnew  -- The most recent sample.
%%                SX    -- The sum of all samples used when calculating Wn, i.e.,
%%                         including Xold, but not Xnew.
%%                Xold  -- The oldest sample, i.e., the sample in turn to be 
%%                         replaced by Xnew.
%%                N     -- The number of samples the calculations are based on.
%%======================================================================

uwma_mean(Xnew, SX, Xold, N) when integer(N), N > 0 ->
    Sum = Xnew + SX - Xold,
    {Sum / N, Sum};
uwma_mean(_Xnew, _SX, _Xold, N) ->
    {error, {too_few_samples, N}}.



%%======================================================================
%% Function:      uwma_variance
%%
%% Return Value:  {EstimatedVariance, SampleSum, SampleSquareSum}  |  
%%                {error, Reason}
%%
%% Description:   This function computes the variance of a number of samples 
%%                using the Uniformaly Weighted Moving Average technique. 
%%                Suppose that [X1, X2, X3, ..., Xn] are random variables 
%%                (observations) with finite population mean u and finite 
%%                population variance V, and sample sum SX, and sample square 
%%                sum SqSX,
%%                When we receive a new sample Xnew, the new estimate of the 
%%                variance will be 
%%                Snew = ((Xnew^2 + SqSX - Xold^2) - (Xnew + SX - Xold)^2 / N) / (N - 1), 
%%                where Xold is the oldest sample 
%%                used in the calculation of SX (i.e., the sample that will be 
%%                replaced by Xnew when calculating Snew), and N is the number of 
%%                samples the calculation of Snew is based on.
%%
%% Parameters:    Xnew  -- The most recent sample.
%%                SX    -- The sum of all samples used when calculating Wn, i.e.,
%%                         including Xold, but not Xnew.
%%                SqSX  -- The square sum of all samples used when calculating Wn,
%%                         i.e., including Xold, but not Xnew.
%%                Xold  -- The oldest sample, i.e., the sample in turn to be 
%%                         replaced by Xnew.
%%                N     -- The number of samples the calculations are based on.
%%======================================================================

uwma_variance(Xnew, SX, SqSX, Xold, N) when integer(N), N > 1 ->
    Sum   = Xnew + SX - Xold,
    SqSum = (Xnew * Xnew) + SqSX - (Xold * Xold),
    {(SqSum - (Sum*Sum) / N) / (N - 1), Sum, SqSum};
uwma_variance(_Xnew, _SX, _SqSX, _Xold, N) ->
    {error, {too_few_samples, N}}.




%%%*********************************************************************
%%%  INTERNAL FUNCTIONS
%%%*********************************************************************

%%======================================================================
%% Function:      sum_and_length
%%
%% Return Value:  {SampleSum, NumberOfSamples}  |  
%%                {error, Reason}
%%
%% Description:   Sums a list of samples, and also computes the number of samples.
%%
%% Parameters:    L  -- List of samples (integers or floats).
%%======================================================================

%sum_and_length([]) ->
%    {error, undefined_operation};
%sum_and_length(L) when list(L) ->
%    sum_and_length(L, 0, 0);
%sum_and_length(L) ->
%    {error, {not_a_list, L}}.


sum_and_length([H | T], Sum, Length) ->
    sum_and_length(T, Sum+H, Length+1);
sum_and_length([], Sum, Length) ->
    {Sum, Length}.



%%======================================================================
%% Function:      sum_squaresum_and_length
%%
%% Return Value:  {SampleSum, SampleSquareSum, NumberOfSamples}  |  
%%                {error, Reason}
%%
%% Description:   Sums a list of samples, sums the square of all samples,
%%                and also computes the number of samples.
%%
%% Parameters:    L  -- List of samples (integers or floats).
%%======================================================================

%sum_squaresum_and_length([]) ->
%    {error, undefined_operation};
%sum_squaresum_and_length(L) when list(L) ->
%    sum_squaresum_and_length(L, 0, 0, 0);
%sum_squaresum_and_length(L) ->
%    {error, {not_a_list, L}}.


sum_squaresum_and_length([H | T], Sum, SqSum, Length) ->
    sum_squaresum_and_length(T, Sum+H, SqSum+(H*H), Length+1);
sum_squaresum_and_length([], Sum, SqSum, Length) ->
    {Sum, SqSum, Length}.



