Data from "Do nest boxes breed the target species or its competitors? A case study of a critically endangered bird"
Authors: Dejan Stojanovic, Giselle Owens, Catherine Young, Fernanda Alves, Robert Heinsohn

#-----------------------

Statistical Methods
We used R program for all analyses (R Development Core Team 2020), and compared competing models using ΔAIC
(Akaike information criterion) <2 (Burnham &Anderson 2002), and visualized the data with ggplot2 (Wickham 2016). We
implemented generalized linear models for each species separately, and included occupancy of nest boxes (0/1) by each
species as response variables with a binomial error distribution. For each species, we fitted a null model and models with the following
fixed effects: distance to forest edge, year, distance to forest edge × year, and distance to forest edge + year. We predicted
occupancy probabilities from the preferred model using the package emmeans (Lenth 2018).

#-----------------------

Data File: occdist.csv.

Data contains the occupancy status of swift parrot nest boxes over two years (2016 and 2019) and the distance (m) of each box from the forest edge

#-----------------------

Dataset contains:
-	287 observations (rows) + 1 row (header)
-	10 variables (columns)

#-----------------------

Variables	Format		Description
box		categorical	Nest box ID (repeated across years)
box occupant	categorical	Species that occupied the box
edge distance	numeric		Distance (m) to the edge of the forest
season		categorical	Year that the record was made
eventID		categorical	Unique alphanumberic number for each record
repeated use	categorical	Yes or No if the box was occupied more than once
sp		categorical	1 = swift parrot 	0 = other species (or empty)
cs		categorical	1 = common starling 	0 = other species (or empty)
e		categorical	1 = unoccupied (empty) 	0 = occupied
tm		categorical	1 = tree martin		0 = other species
