ramp <-
function (n) 
{
    x <- ramp(seq.int(0, 1, length.out = n))
    rgb(x[, 1], x[, 2], x[, 3], maxColorValue = 255)
}
