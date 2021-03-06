using DataFrames: Not, DataFrame, unstack, stack, groupby, eachrow, combine
using StatsBase: StatsBase, autocor, cor, quantile, sample
using AlgebraOfGraphics: AlgebraOfGraphics, data, dims, mapping, visual, categorical
using AbstractPlotting: Lines, Scatter, BarPlot, Errorbars, Heatmap
using AbstractPlotting.MakieLayout
using MCMCChains: cummean

function paramframe(modelchains)
    df = DataFrame(modelchains[:, names(modelchains, [:parameters]), :])
    stacked = stack(df, Not([:iteration, :chain]))
end

# TODO Show hpd region
function plot_density(mc; args...)
    (
        data(paramframe(mc))
        * mapping(:value, color=:chain => categorical, layout_y=:variable)
        * AlgebraOfGraphics.density(;args...)
    )
end


function plot_histogram(mc; args...)
    (
        data(paramframe(mc))
        * mapping(layout_y=:variable)
        * mapping(:value)
        * AlgebraOfGraphics.histogram(;args...)
    )
end

function plot_trace(mc)
    (
        data(paramframe(mc))
        * mapping(layout_y=:variable)
        * mapping(:iteration, :value)
        * mapping(color=:chain => categorical)
        * visual(Lines)
    )
end

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

# Broken
function plot_parameter_correlation(mc)
    df = unstack(paramframe(mc))
    df = df[:, names(mc, :parameters)]
    df2 = DataFrame(cor(Array(df)))
    df2 = rename(df2, names(df))
    df2[:, :x] = names(df)
    df = stack(df2, Not(:x))
    df = rename(df, :variable => :y)
    data(df) * mapping(:x => categorical, :y => categorical, color=:value => categorical) * visual(Heatmap)
end



# Change x axis label
function plot_coefficients(mc)
    df = paramframe(mc)
    df = groupby(df, :variable)
    df = combine(df,
                 :value => (x->quantile(x, 0.5)) => :q50,
                 # :value => (x->quantile(x, .025)) => :q025,
                 # :value => (x->quantile(x, .25)) => :q25,
                 # :value => (x->quantile(x, .75)) => :q75,
                 # :value => (x->quantile(x, .975)) => :q975,
                 :value => (x->quantile(x, .75) - quantile(x, .5)) => :e75_50,
                 :value => (x->quantile(x, .5) - quantile(x, .25)) => :e25_50,
                 :value => (x->quantile(x, .975) - quantile(x, .5)) => :e975_50,
                 :value => (x->quantile(x, .5) - quantile(x, .025)) => :e025_50,
                 )
    df = rename(df, :variable => :parameter)

    (
        data(df) * (
            mapping(:q50, :parameter) * visual(Scatter, markersize=6)
            + mapping(:q50, :parameter, :e25_50, :e75_50) * visual(Errorbars, direction=:x, linewidth=4, whiskerwidth=0)
            + mapping(:q50, :parameter, :e025_50, :e975_50) * visual(Errorbars, direction=:x, color=:gray)
        )

    )
end

function plot_chains_parallel(modelchains)
    df = paramframe(modelchains)
    df = transform(df, [:iteration, :chain] => ByRow(tuple) => :id)
    df = DataFrame(sample(eachrow(df), 1000))
    p = dims()
    groups = groupby(df, :id)
    (
        data(df)
        * mapping(:variable => categorical, :value, group = :id => categorical)
        * visual(Lines)
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
