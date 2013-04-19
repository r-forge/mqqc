col.grad <- 
function(test = best,best = 1,bad = 0, col = c("blue","orange","yellow","green"),res = 100)
{
	
col.ramp 	<- rev(colorRampPalette(col)(res))
grad 		<- seq(best,bad, length.out = res)



if(best > bad){
	if(test > best){
		x.col <- col[length(col)]
	}else{
	if(test < bad){
			x.col <- col[1]
		}else{
	
			x.col  <- col.ramp[test <= grad ]
			x.col <- x.col[length(x.col)]
		}
	}

}

if(best < bad){
	if(test < best){
					x.col <- col[length(col)]

	}else{
		if(test > bad){
				x.col 	<- col[1]

		}else{
			x.col <- col.ramp[test >= grad ]
			x.col <- x.col[length(x.col)]
		}
	}
}

	
	return(list(col = x.col,ramp = col.ramp))
}
