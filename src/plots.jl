using DataFrames: Not, DataFrame, stack
using AlgebraOfGraphics
using AbstractPlotting
using AbstractPlotting.MakieLayout


function paramframe(modelchains)
    df = DataFrame(modelchains[:, names(modelchains, [:parameters]), :])
    stacked = stack(df, Not([:iteration, :chain]))
end

# TODO Show hpd region
function mcdensity(mc, args...)
    (
        data(paramframe(mc))
        * mapping(:value, color=:chain => categorical, layout_y=:variable)
        * AlgebraOfGraphics.density(args...)
    )
end


function plot_histogram(mc, args...)
    (
        data(paramframe(mc))
        * mapping(layout_y=:variable)
        * mapping(:value)
        * AlgebraOfGraphics.histogram(args...)
    )
end

function plot_trace(mc)
    (
        data(paramframe(mc))
        * mapping(layout_y=:variable)
        * mapping(:iteration, :value)
        * mapping(color=:chain )
        * visual(Lines)
    )
end

cummean(A) = cumsum(A) ./ (1:length(A))

function plot_cummean(mc)
    df = paramframe(mc)
    df = groupby(df, :variable)
    df = transform(df, :value => cummean => :cummean)
    (
        data(df)
        * mapping(:iteration, :cummean, layout_x=:chain =>categorical, layout_y=:variable)
        * visual(Lines)
    )
end

function plot_partial_complete(mc)
    df = paramframe(mc)
    df[:, :length] .= "Complete"
    partial = filter(x->x.iteration >= size(mc,1)*.9, df)
    partial[:, :length] .= "Partial"
    df = vcat(df, partial)
    (
        data(df)
        * mapping(:value, color=:length => categorical, layout_y=:variable => categorical)
        * AlgebraOfGraphics.density
    )
end


function plot_autocorrelation(mc)
    df = groupby(paramframe(mc), [:chain, :variable])
    df = combine(
        df,
        :value => autocor => :autocor,
        :value => (x->StatsBase.default_autolags(size(x,1))) => :lag
    )
    data(df) * mapping(:lag, :autocor,  layout_x=:chain => categorical, layout_y=:variable) * visual(BarPlot)
end


function plot_parameter_correlation(mc)
    df = unstack(paramframe(mc))
    df = df[:, names(mc, :parameters)]
    df2 = DataFrame(cor(Array(df)))
    df2 = rename(df2, names(df))
    df2[:, :x] = names(df)
    df = DataFrames.stack(df2, Not(:x))
    df = rename(df, :variable => :y)
    data(df) * mapping(:x => categorical, :y => categorical, color=:value) * visual(Heatmap)
end


# caterpillar/coefficients
# Why is the 2mad smaller??
# Change x axis label
function plot_coefficients(mc)
    df = paramframe(mc)
    df = groupby(df, :variable)
    df = combine(df,
                 :value => (x->quantile(x, 0.5)) => :q50,
                 :value => (x->quantile(x, .025)) => :q025,
                 :value => (x->quantile(x, .25)) => :q25,
                 :value => (x->quantile(x, .75)) => :q75,
                 :value => (x->quantile(x, .975)) => :q975,
                 )
    df = rename(df, :variable => :parameter)
    (
        data(df) * (
            mapping(:q50, :parameter) * visual(Scatter)
            + mapping(:q50, :parameter, :q25, :q75) * visual(Errorbars, direction=:x, linewidth=4, whiskerwidth=0)
            + mapping(:q50, :parameter, :q025, :q975) * visual(Errorbars, direction=:x, color=:red)
        )

    )
end

# mcrhat
# geweke
# ppmean
# ppsd
# rocplot
# separation
# pcp
# pairs
# Nuts energy diagnostic (bayesplot.R)
# Density overlay (bayesplot.R)
# y/yrep for coefplot (bayesplot.R)

# Inspiration:
# Arviz
# ggmcmc
# bayesplot.R
