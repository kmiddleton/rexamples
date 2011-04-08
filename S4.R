setClass('Rectangle', representation(h = 'numeric', w = 'numeric', area = 'numeric'))

myr <- new('Rectangle', h = 10, w = 20, area = 200)

setGeneric('area', function(shape) standardGeneric('area'))

setMethod('area', signature(shape = 'Rectangle'), function(shape) shape@area)

myr@area
area(myr)

setClass('Rectangle', representation(h = 'numeric', w = 'numeric'))
setMethod('area', 'Rectangle', function(shape) shape@h * shape@w)

myr <- new('Rectangle', h = 10, w = 20)
area(myr)
