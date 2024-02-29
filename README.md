### Description
cascAGS is a program that estimates the sensitivity and specificity of variant calling methods based on latent class models (LCMs) to evaluate their accuracy in the absence of SNP gold standards. It can be used to process input files in Excel or VCF format, generating sensitivity and false positive rate (Specificity=1–false positive rate) estimates under assumptions of conditional independence and conditional dependence (including constant random effects and variable random effects).

cascAGS is suitable for the accuracy evaluation of three calling methods under conditional independence assumption, and accuracy evaluation under any assumption of more than four calling 
methods, where LCM estimates each parameter using the maximum likelihood method.

### How to run cascAGS
We recommend using R. You can download the complete repository and unzip it. Functions for the interactive page are defined in the files within the <span style="color: blue; font-family: Consolas;"> R</span> folder. To compare calling methods, 
the readers can do the following:

1. Click the <span style="color: blue; font-family: Consolas;">Run App</span> on the <span style="color: blue; font-family: Consolas;">APP.R</span> to obtain the webpage;

2. Upload your Excel file through <span style="color: blue; font-family: Consolas;">Browse</span>, or click <span style="color: blue; font-family: Consolas;">Preprocessor</span> to download <span style="color: blue; font-family: Consolas;">Preprocessor.R</span> to convert your VCF, then upload the output CSV file through <span style="color: blue; font-family: Consolas;">Browse</span>. In <span style="color: blue; font-family: Consolas;">Preprocessor.R</span>, you only need to change the VCF file path on <span style="color: red; font-family: Consolas;">line 32</span> to your local save path and run the entire R file to obtain the processed CSV output.

3. Input the total number of bases or individuals on <span style="color: blue; font-family: Consolas;">Reference Base Count</span>;

4. Click <span style="color: blue; font-family: Consolas;">Run Analysis</span> to start the analysis.

You should now see output similar to the example results by clicking the <span style="color: blue; font-family: Consolas;">Run Example</span> at https://lish-bioinfo.shinyapps.io/cascAGS/. Being able to produce the above output shows that your system is up and running.

### Example data

To illustrate its practical application, we provide three example data, namely, the third-generation sequencing data of a breast cancer patient used in this paper, the sequencing data of chromosome 22 from PrecisionFDA, and the Uterine cardiovascular data, from seven pathologists. The total number of bases or patients (namely, Refcount) involved is provided under each example. Readers can click <span style="color: blue; font-family: Consolas;">Download Example</span> to download example data and view the data format. Or click <span style="color: blue; font-family: Consolas;">Run Example</span> to run the data and obtain the corresponding analysis results. When calculating the confidence interval of the parameters as in the main text, due to the introduction of the bootstrap method, the estimated value is slightly smaller than the directly calculated value here, but it does not affect the final accuracy ranking.

<img src="./Figures/Example_data.png" alt="Fig.1 Example data and its description" style="width:800px; height:auto; background-color: transparent;">

### Analysis Results & Visualization
#### Data Upload & Settings
In this section, you can upload your file and set the total number of base counts or individuals involved in your trial.

<img src="./Figures/Data_input.png" alt="Fig.2 Data upload and the total number of bases settings" style="width:800px; height:auto; background-color: transparent;">

#### Venn Diagram
In this section, we present the number of SNPs detected by all calling methods, and the consistency between different methods can be seen from the diagram. The SNPs corresponding to each number in the Venn plot are summarized in a table. Readers can click <span style="color: blue; font-family: Consolas;">Download CSV</span> to view the chromosomes and coordinates of all corresponding SNPs and click <span style="color: blue; font-family: Consolas;">Download PDF</span> to obtain the diagram.

<img src="./Figures/Venn_diagram.png" alt="Fig.3 Venn diagram of intersection" style="width:800px; height:auto; background-color: transparent;">

#### Sensitivity & False Positive Rate
In this section, we present the sensitivity and false positive rate of all calling methods estimated using the latent class model under the assumptions of conditional independence and conditional dependence (including constant random effects and variable random effects) without gold standards and specificity=1-false positive rate. The LCMs corresponding to the three assumptions are named LCMC, LCR, and LCMR, respectively. Readers can click <span style="color: blue; font-family: Consolas;">Download CSV</span> to obtain all estimated values, and select the optimal estimated value based on the results of the <span style="color: blue; font-family: Consolas;">Residual Analysis</span> section as the sensitivity and false positive rate estimate for calling the method.

<img src="./Figures/Sensitivity_false_positive_rate.png" alt="Fig.4 The estimated sensitivity and false positive rate under three assumptions" style="width:800px; height:auto; background-color: transparent;">

#### Residual Analysis
In this section, we present the residuals corresponding to the three assumptions mentioned earlier. For any two calling methods, we calculate the Pearson correlation coefficients of the observed frequency and the expected frequency, respectively. The expected frequency is obtained through LCM, and the difference between the two is called the residual. <span style="color: red; font-family: Arial;">The closer the residual is to zero, the more accurate the estimate obtained based on this assumption.</span> Readers can also obtain this graphic by clicking <span style="color: blue; font-family: Consolas;">Download PNG</span> or <span style="color: blue; font-family: Consolas;">Download PDF</span>.

<img src="./Figures/Residual_plot.png" alt="Fig.5 Correlation residual style="width:800px; height:auto; background-color: transparent;">

#### SNP Count Estimation
In this section, we present the observed SNP frequency and the expected frequency estimated by LCM under three assumptions to illustrate the impact of dependency assumptions on the performance of LCM estimation. In addition, readers can check the original function themselves to obtain the class probability estimated by LCM to judge the accuracy of single or two calling methods. The final results can be obtained by clicking <span style="color: blue; font-family: Consolas;">Download CSV</span>.

<img src="./Figures/Observed_expected_freqency.png" alt="Fig.6 The observed SNP frequency and the expected frequency estimated by LCM" style="width:800px; height:auto; background-color: transparent;">


