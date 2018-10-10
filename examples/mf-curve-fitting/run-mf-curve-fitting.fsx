#I "../../packages"
#r "Google.DataTable.Net.Wrapper/lib/Google.DataTable.Net.Wrapper.dll"
#r "XPlot.GoogleCharts/lib/net45/XPlot.GoogleCharts.dll"
#r "XPlot.GoogleCharts.WPF/lib/net45/XPlot.GoogleCharts.WPF.dll"
#load "mf-curve-fitting.fs"

open XPlot.GoogleCharts

open MF_CurveFitting

[| 1 .. 10 |] |> Array.map (fun i -> i,(float i)**3.) |> Chart.Line |> Chart.Show



