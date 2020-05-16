a <-  "Na Na Na Na Batman!"

b <- "Na Batman!"

c <-  "Na Na Na Na Na Na Na Na Batman!"

d <-  "Batman!"

e <-  "Na Na Na Spiderman!"

f <-  "NaNaNaNa Batman!"

g <-  "Na,Na,Na,Na,Batman!"


str_view(f,"(Na\\s){1,}Na\\sBatman\\!")

str_view(a,"(Na\\s){1,}Na\\sBatman\\!|^(Na\\sBatman\\!)")
str_view(b,"(Na\\s){1,}Na\\sBatman\\!|^(Na\\sBatman\\!)")
str_view(c,"(Na\\s){1,}Na\\sBatman\\!|^(Na\\sBatman\\!)")
str_view(d,"(Na\\s){1,}Na\\sBatman\\!|^(Na\\sBatman\\!)")
str_view(e,"(Na\\s){1,}Na\\sBatman\\!|^(Na\\sBatman\\!)")
str_view(f,"(Na\\s){1,}Na\\sBatman\\!|^(Na\\sBatman\\!)")
str_view(g,"(Na\\s){1,}Na\\sBatman\\!|^(Na\\sBatman\\!)")


str_detect(a,"(Na\\s){1,}Na\\sBatman\\!")
str_detect(b,"(Na\\s){1,}Na\\sBatman\\!")
str_detect(c,"(Na\\s){1,}Na\\sBatman\\!")
str_detect(d,"(Na\\s){1,}Na\\sBatman\\!")
str_detect(e,"(Na\\s){1,}Na\\sBatman\\!")
str_detect(f,"(Na\\s){1,}Na\\sBatman\\!")
str_detect(g,"(Na\\s){1,}Na\\sBatman\\!")


mail1 <-  "a@b.c"
mail2 <-  "AsDf@JkL.QwEr"
mail3 <-  "ab@cd"
mail4 <-  "xy.z"
mail5 <-  "a1@b2.c3"
mail6 <-  "@a."


str_view(mail1,"[:alpha:]{1,}\\@[:alpha:]{1,}\\.[:alpha:]{1,}")
str_view(mail2,"[:alpha:]{1,}\\@[:alpha:]{1,}\\.[:alpha:]{1,}")
str_view(mail3,"[:alpha:]{1,}\\@[:alpha:]{1,}\\.[:alpha:]{1,}")
str_view(mail4,"[:alpha:]{1,}\\@[:alpha:]{1,}\\.[:alpha:]{1,}")
str_view(mail5,"[:alpha:]{1,}\\@[:alpha:]{1,}\\.[:alpha:]{1,}")
str_view(mail6,"[:alpha:]{1,}\\@[:alpha:]{1,}\\.[:alpha:]{1,}")



zahl1 <- "zahl1234"
zahl2 <- "anzahl123" 
zahl3 <- "zahl123M"
zahl4 <- "zahl0987" 
zahl5 <- "zahl987" 
zahl6 <- "zahl1.2" 

str_view(zahl1,"^(zahl[1-9]{1,})")
str_view(zahl2,"^(zahl[1-9]{1,})")
str_view(zahl3,"^(zahl[1-9]{1,})")
str_view(zahl4,"^(zahl[1-9]{1,})")
str_view(zahl5,"^(zahl[1-9]{1,})")
str_view(zahl6,"^(zahl[1-9]{1,})")

str_detect(zahl1,"^(zahl[1-9]{1,})$")
str_detect(zahl2,"^(zahl[1-9]{1,})$")
str_detect(zahl3,"^(zahl[1-9]{1,})$")
str_detect(zahl4,"^(zahl[1-9]{1,})$")
str_detect(zahl5,"^(zahl[1-9]{1,})$")
str_detect(zahl6,"^(zahl[1-9]{1,})$")



zu1 <- "zuerst a dann b"
zu2 <- "zuerst bla dann blub"
zu3 <- "zuerst zuerst dann dann"

str_split_fixed(zu1," ",4)[1,][c(1,4,3,2)]



mir1 <- "Mir geht es gut."
mir2 <- "Mir geht es so lala."
mir3 <- "Mir geht es eher mittel."
mir4 <- "Mir geht es wundervoll."

str_extract(mir1,"(?<=Mir\\sgeht\\ses\\s)[^\\.]{1,}")
str_extract(mir2,"(?<=Mir\\sgeht\\ses\\s)[^\\.]{1,}")
str_extract(mir3,"(?<=Mir\\sgeht\\ses\\s)[^\\.]{1,}")
str_extract(mir4,"(?<=Mir\\sgeht\\ses\\s)[^\\.]{1,}")



