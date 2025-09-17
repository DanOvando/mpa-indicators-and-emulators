#snapper / rockfish complex (mediun momvement medium dispersal)
# silky shark (high movement low dispersal)
# yellowfin tuna (high movement high dispersal)
# lobster / octopus / low movement high dispersal
# something low movement low dispersal (octopus?)
# 
# 

#WCPFC-SC20-2024/SA-WP-04-Rev2
#
library(marlin)
linf <- 216
k <- .148
ages <- 0:25

length = linf * (1 - exp(-k * ages))
plot(ages, length)

t0 <- log(1 - 50 / linf) / k
length = linf * (1 - exp(-k * (ages - t0)))

plot(ages, length)


#Oshitani, Shungo, Hideki Nakano, and Sho Tanaka. “Age and Growth of the Silky Shark Carcharhinus Falciformis from the Pacific Ocean.” Fisheries Science 69, no. 3 (2003): 456–64. https://doi.org/10.1046/j.1444-2906.2003.00645.x.
# Neubauer, P, K Kim, K Large, and S Brouwer. “Stock Assessment of Silky Shark in the Western and Central Pacific Ocean.” Report to the Western and Central Pacific Fisheries Commission Scientific Committee. Twentieth Regular Session, 2024.

length_at_age <- 216.4 * (1 - exp(-0.148 * ((0:30) - -1.778991)))

pups_at_age <- pmax(0,-8.6 + 0.098 * length_at_age)

shark <- marlin::create_critter(
  query_fishlife = FALSE,
  scientific_name = "Carcharhinus falciformis",
  common_name = "silky shark",
  m = 0.21,
  lorenzen_c = -.5,
  linf = 216.4,
  vbk = 0.148,
  t0 = -1.778991, # 50cm at birth
  min_age = 0,
  max_age = 30,
  age_mature = 5,
  weight_a = 0.0000273,
  weight_b = 2.86,
  fec_at_age = pups_at_age)

shark$plot()

 tuna <- marlin::create_critter(
   query_fishlife = FALSE,
   scientific_name = "Thunnus albacares",
   common_name = "yellowfin tuna",
   m = 0.4,
   growth_model = "growth_cessation",
   lorenzen_c = -1,
   l0 = 18.85,
   rmax = 37.24,
   k =  0.89,
   t50 = 4.57,
   min_age = 0,
   max_age = 15,
   age_mature = 3,
   weight_a = 0.00004,
   weight_b = 2.86)
 
 tuna$plot()
 
 
 grouper <- marlin::create_critter(
   query_fishlife = FALSE,
   scientific_name = "Epinephelus fuscoguttatus",
   common_name = "goliath grouper",
   m = 0.19,
   lorenzen_c = -.75,
   linf = 222.11,
   vbk = 0.0937,
   t0 = -.68, 
   min_age = 0,
   max_age = 40,
   age_mature = 6,
   weight_a = 1e-5,
   weight_b = 3.151)
 

 
 rockfish <- marlin::create_critter(
   query_fishlife = FALSE,
   scientific_name = "Sebastes mystinus",
   common_name = "blue rockfish",
   m = 0.14,
   lorenzen_c = -.75,
   linf = 40,
   vbk = 0.147,
   t0 = -2, 
   min_age = 0,
   max_age = 50,
   age_mature = 3,
   weight_a = 1e-3,
   weight_b = 2.6)
 
 
 rockfish$plot()
 