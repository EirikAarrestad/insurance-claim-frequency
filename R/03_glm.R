train <- readRDS("data/train.rds")
test  <- readRDS("data/test.rds")

glm(ClaimNb ~ DrivAge + VehPower + Area + offset(log(Exposure)), family = poisson, data = train)
?glm
