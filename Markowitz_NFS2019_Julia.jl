#Test Script to run a Markowitz Model in Julia
#Prepared by Eric Torkia for the Need for Speed 2019 study
#Please cite authorship if you wish to reuse all or some of the contained code.


clearconsole()

#Step 1 - Load Data

using DelimitedFiles;
using CSV;
using DataFrames;
using Statistics;
using Dates;
using StatsBase;
using LinearAlgebra;
using Distributions;
using Base;
using Distributed;
using MCHammer
Start_Time= Dates.Time(Dates.now())

#Set Global Parameters
Run_Scenarios = [10000, 50000, 100000]
Initial_Value = 250000
Model_Path = "c:\\tmp\\"
#Load Returns from CSV
CSV_Path = "c:\\tmp\\Test_Returns.csv";
Returns_DF = CSV.read(CSV_Path)
Returns_DF = Returns_DF[2:5]
Returns_Data = convert(Matrix, Returns_DF)

#CALCULATED SOLUTION USING MARKOWITZ (1952)
# This function calculates the mean return per asset class using DataFrames, LinearAlgebra, Statistics

mean_return_all = []
for i=1:ncol(Returns_DF)
       mean_return= mean(Returns_DF[i]);
       push!(mean_return_all,mean_return)
end

#Strangely, when calling an Array[], it only produces the first value. You would need to know the length before hand. If not, use the DataFrame. This is especially true for Spearman and Standard Deviation.

volatility_all = []
for i=1:ncol(Returns_DF)
       volatility_i = std(Returns_DF[i]);
       push!(volatility_all,volatility_i)
end

#Prep Covariance Results
cov_mat = cov(Returns_Data)

# Generate the correlation matrix. Replaced homemade function with built-in
cor_mat = cormat(Returns_DF,1)

#Create Allocations Array (***If you add commas, the its  a matrix, without comma its an array. This can mess with your calculations)
Allocations_S = [0.25 0.25 0.25 0.25] #All values are iterated 1by1 in col - without the commas
Allocations_Sm = [0.25, 0.25, 0.25, 0.25] #All values are iterated 1by1 in a row
Allocations = fill(0.25,4,4)

#Calculate Theoretical Portfolio Stats
Portfolio_Var = mean((Allocations * cov_mat)*Allocations_Sm)
Portfolio_Std = sqrt(Portfolio_Var)
Portfolio_Mean = mean(Allocations * mean_return_all)

# print("\n")
# print("PORTFOLIO STATS")
# print("\n")
# print("Portfolio Var: ",Portfolio_Var*100,"%", "\n" )
# print("Standard. Dev: ",Portfolio_Std*100,"% ","\n")
# print("Portfolio Mean: ", Portfolio_Mean*100,"%","\n")

#Calc Theoretical Percentiles

#using Distributions, Statistics, StatsBase
Portfolio_Returns_Dist = Normal(Portfolio_Mean,Portfolio_Std)
Dist_Pvalues = [0.01,0.05,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95,0.99,0.999,0.9999,0.99999,0.999999 ]

Calculated_Portfolio_Results = quantile.(Portfolio_Returns_Dist,Dist_Pvalues)

# print("\n")
# print("\n")
# print("Calculated Percentiles")
# print("\n")
# println.(Dist_Pvalues, ": ", Calculated_Portfolio_Results)

#Sim Based Model
#Loop Results for 1000, 5000, 10000, 100000, 1000000

DF_Header = ["Mean"	"Std.Dev"	"Min"	"Max"	"P1"	"P5"	"P10"	"P20"	"P30"	"P40"	"P50"	"P60"	"P70"	"P80"	"P90"	"P95"	"P99"	"P99_9"	"P99_99"	"P99_999"	"P99_9999"	"Trials"	"Duration_ms"]


DF_Results = []
push!(DF_Results, DF_Header)

#Generate Array of Distributions
Asset_Names = []
for i = 1:length(mean_return_all)
      Asset_Name = string("Asset_",i)
      push!(Asset_Names,Asset_Name)
end

Assets = Asset_Names
for i=1:length(Asset_Names)
      Assets[i] = Normal(mean_return_all[i],volatility_all[i])
end

#addprocs(6)

for i = 1:length(Run_Scenarios)
       Run_Trials = Run_Scenarios[i]
       @sync @distributed for SimID = 1:100
             #Create Returns SIP - Put Simulation Model Code Here
             #Time Stamp the start of the simulation
            Start_Time_Sim = Dates.Time(Dates.now())

            #Generate Samples from Distributions Array (Assets)
            Sim_Trials =[]
            for i = 1:length(Assets)
                  Asset_Sim_Trials = rand(Assets[i],Run_Trials)
                  push!(Sim_Trials,Asset_Sim_Trials)
            end


            #Correlate Inputs using Iman-Conover function corvar(ArrayName, n_trials, correl_matrix)
            Results = corvar(Sim_Trials, Run_Trials, cor_mat)
            Results = convert(Array, Results)
            Ones_Arr = fill(1, size(Results))
            PortValue_Arr = fill(Initial_Value, size(Results))
            Alloc_Arr = fill(Allocations_S, size(Results)[1])

            #Capture Results into Array. This where the Monte-Carlo function resides

            global Portfolio_Sim_Arr =[]
            #PRoduce Results
            Portfolio_Sim_Arr = (Ones_Arr+Results) .*PortValue_Arr
            Portfolio_Sim_Arr = Portfolio_Sim_Arr * Allocations_Sm
            End_Time = Dates.Time(Dates.now())

            #Calc Array Stats from Simulation
            Portfolio_Sim_Mean = mean(Portfolio_Sim_Arr)
            Portfolio_Sim_Std = std(Portfolio_Sim_Arr)
            Portfolio_Sim_Min = minimum(Portfolio_Sim_Arr)
            Portfolio_Sim_Max = maximum(Portfolio_Sim_Arr)
            PS = quantile(collect(Float64, Portfolio_Sim_Arr),Dist_Pvalues)

            #Create DataFrame LinearAlgebra

            #Setup Array to push into DataFrame

            DF_Line = []

            push!(DF_Line, Portfolio_Sim_Mean)
            push!(DF_Line, Portfolio_Sim_Std )
            push!(DF_Line, Portfolio_Sim_Min )
            push!(DF_Line, Portfolio_Sim_Max )
            for i = 1:length(PS)
                   push!(DF_Line, PS[i])
            end
            push!(DF_Line, Run_Trials )
            push!(DF_Line, convert(Dates.Millisecond, Dates.Nanosecond(End_Time-Start_Time_Sim)).value)
            #push!(DF_Line, (End_Time-Start_Time_Sim))

            @sync push!(DF_Results, DF_Line)



            #print(summarystats(collect(flatten(Portfolio_Sim_Arr))))

            # print("\n")
            # print("\n")
            # print("Percentiles",)
            # print("\n")
            # println.(Dist_Pvalues, ": ", PS)
            #
            # #Collect Time Math
            print("Simulation: ",i,"_",SimID,"\n")
            print("Run Trials: ",Run_Trials)
            #print("\n")
            Sim_Duration = convert(Dates.Millisecond, Dates.Nanosecond(End_Time-Start_Time_Sim))

            Total_Duration = convert(Dates.Millisecond, Dates.Nanosecond(End_Time-Start_Time))
            Total_Trials_Run = sum(Run_Scenarios)*20
            Duration_Sec = Total_Duration.value / 1000
            Duration_Min = Duration_Sec / 60
            Trials_Sec = Total_Trials_Run / Duration_Sec

            #Report Performance
            print("\n")
            print("Sim Duration: ", Sim_Duration)
            print("\n")

            print("Total Duration in Seconds: ", Duration_Sec)
            print("\n")
            print("Trials / Sec: ", Trials_Sec)

            print("\n")
            print("\n")


            end
      end


# #Portfolio Agregate Sim vs Iman Conover Sim
#       using Gadfly, Compose
#       Run_Trials = 100000
#       Return_1_Arr = fill(1,Run_Trials,1)
#       Portfolio_Sim_Theo = (rand(Portfolio_Returns_Dist,Run_Trials) + Return_1_Arr)*Initial_Value
#
#      plot(x=[Portfolio_Sim_Theo Portfolio_Sim_Arr], Geom.density, color=["Theo Markowitz","Iman-Conover"], Guide.Title("Compare Portfolio Methods"))

#Write Simulation Performance Benchmarks to file
writedlm("c:\\tmp\\df_results.csv",DF_Results, ',')
