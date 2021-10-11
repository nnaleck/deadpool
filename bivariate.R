plotBivariateCloudPoints <- function(first_feature, second_feature)
{
    if (! is.numeric(df[, first_feature]) & ! is.numeric(df[, second_feature]))
    {
        p <- ggplot(df, aes_string(x = first_feature , fill=second_feature)) +
            geom_bar() +
            theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

        return(ggplotly(p))
    }
    else if( is.numeric(df[, first_feature]) &  is.numeric(df[, second_feature]))
    {
        p <- plot_ly(
            x = df[, first_feature],
            y = df[, second_feature],
            name = paste(second_feature, ' according to ', first_feature),
            type = 'scatter',
            mode = 'markers'
        )

        p <- p %>% add_trace(
            x = df[, first_feature],
            y = fitted(lm(df[, second_feature]~df[, first_feature])),
            mode = 'lines',
            name = 'Linear model'
        )

        return(p)
    }
    else if(
        (is.numeric(df[, first_feature]) & !is.numeric(df[, second_feature]))
        |
        (is.numeric(df[, second_feature]) & !is.numeric(df[, first_feature]))
    )
    {
        xlabel <- if(is.numeric(df[, first_feature])) first_feature else second_feature
        ylabel <- if(is.numeric(df[, first_feature])) second_feature else first_feature

        p <- ggplot(data=df)+
            geom_histogram(mapping = aes_string(xlabel, fill=ylabel), bins = 10)+
            xlab(label = xlabel)+
            ylab(label="Frequency")+
            theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

        return(ggplotly(p))
    }
}