module BayesPlots

include("plots.jl")

export plot_density, plot_histogram, plot_trace, plot_cummean, plot_partial_complete, plot_autocorrelation, plot_parameter_correlation, plot_coefficients

# Reexport from AlgebraOfGraphics.
export draw

end
