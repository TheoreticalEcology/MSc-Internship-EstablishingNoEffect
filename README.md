This repository provides all R code for

Isabelle Halbhuber (2023) A comparison of statistical methods to establish no effect.

Additionally it contains the R code for

Mair, MM, Kattwinkel, M, Jakoby, O and Hartig, F (2020) The MDD concept for establishing trust in non-significant results - a critical review. The code is from the GitHub repository 'https://github.com/TheoreticalEcology/Mair-et-al-2020' and you can find it in the directory "Mair-et-al/Mair-et-al-2020-master".

The repository also contains the R code

by Magdalena Mair from Florian Hartig and can be found in the directory "Mair-et-al/Mair-NoEffectSimulations". 

Abstract


The true absence of an effect is not necessarily established by a null hypothesis test result that is non-significant (p-value > significance level). Therefore, various sta?s?cal methods have been developed or used to detect a true 'no effect'. Nevertheless, non-significant results can s?ll be interpreted by post-hoc power analyses, such as calcula?ng the minimum detectable difference (MDD), which indicates whether the experiment could have detected a relevant effect. Apart from MDD, which has limita?ons for post-hoc interpreta?on of non-significant results, other sta?s?cal methods that are used for detec?ng a true 'no effect' may be more appropriate for this purpose. The choice of the correct sta?s?cal test, and hence the detec?on of a true null effect, is par?cularly important in the risk assessment of chemicals. The main impetus for risk assessment, e.g., by the European Food Safety Agency (EFSA), is to provide safe and reliable no-effect concentra?ons of chemicals while minimizing financial investment and ethical implica?ons (e.g., animal tes?ng). However, it is currently unclear which test method provides the most reliable results while requiring the least resources. To answer these ques?ons, I compared the MDD with Confidence Intervals (CI), Bayes factors (BF) and Equivalence test (EQUIV) in their ability to dis?nguish between true and false nega?ves using false trust and false mistrust rates, in this work. Addi?onally, I analyzed how the false trust rate of EQUIV and MDD are impacted by varying sample sizes. The laUer is mo?vated by the recent switch of EFSA from the MDD to EQUIV in their tes?ng protocol. Based on the R script of Mair et al. (2020), I developed two simula?ons, the first compares the false trust and false mistrust rates of all sta?s?cal methods, and the second inves?gates changes in the false trust rate of EQUIV and MDD with varying sample size. To provide a comprehensive understanding, I explain the concept of MDD and CI, the Bayesian approach using BF, and the principles of EQUIV. My main findings are that CI and EQUIV are equivalent and that CI, EQUIV and BF outperform MDD in iden?fying true nega?ves among non-significant results. The reason for this is that MDD, unlike CI, EQUIV and BF, does not consider the es?mated effect size in its calcula?on. This also explains why the old EFSA guidelines, which used MDD, required fewer animals to be tested than EQUIV. I found that EQUIVs achieve a 70% false trust rate with a sample size of 30, while MDDs reach a 70% false trust rate with a sample size of 6. EQUIV requires a larger sample size in risk assessment but ensures more reliable results compared to MDD. In conclusion, I recommend the use of CIs for deciding whether to treat a non-significant test result as a true nega?ve. CIs are more robust to changes in sample size than BFs and, unlike EQUIVs, are a common method that most scien?sts have in their sta?s?cal toolbox. This makes CIs the most appropriate method for establishing no effect.



The code for the two simulations:

The R script "FMR_FTR_simulation" is designed to perform hypothesis testing and trust analysis for different statistical methods, including Minimum Detectable Difference (MDD), Confidence Interval (CI), Equivalence tests (EQUIV), Bayes Factor Ratio (BFRatio), and random value testing. It evaluates the trust in the "no effect" hypothesis using various statistical methods.

The R script "sampleSize_simulation" is designed to perform hypothesis testing and trust analysis for MDD and EQUIV. It evaluates the trust in the "no effect" hypothesis using MDD and EQUIV for different data sets displaying different sample sizes. 


Key components of the scripts include:

Data Generation: The script generates data sets with different effect sizes and sample sizes using the 'CreateSampleDataset' function.
Simulation Settings: It defines a grid of simulation settings, including methods (MDD, CI, EQUIV, BFRatio, random_values), threshold values, and dataset indices.
Parallel Processing: The script utilizes parallel processing for efficient computation by creating a cluster with 5 nodes.
Hypothesis Testing: It performs hypothesis testing for each simulation setting, calculates p-values, and determines whether the effect is detected.
Trust Analysis: The script calculates trust values for different methods and threshold levels and stores the results.
Aggregate and Visualize: It aggregates the results and visualizes the trust in the "no effect" hypothesis for each method. The 'plotResult' function is used to create line graphs that display False Mistrust Rates (FMR) against False Trust Rates (FTR) or "No effect trusted" against "dataset".

Usage: To use the script, you can modify the parameters in the 'CreateSampleDataset' function and execute the subsequent simulations. You can also customize the visualization of trust results by calling the 'plotResult' function with different methods.

External function: The two scripts have the option to use the function 'MDD_function_Mair.R', which comes from an external GitHub repository. This function provides specific calculations needed in the context of this project.

The source code file and more details about 'MDD_function_Mair.R' can be found in the Git repository 'https://github.com/TheoreticalEcology/Mair-et-al-2020'.

Dependencies

The script relies on the 'BayesFactor' library for Bayes Factor Ratio calculations.
The R Version 2023.06.2+561 (2023.06.2+561) was used.

License

This script is available under the MIT License.
