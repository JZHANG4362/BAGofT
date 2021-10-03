
load(file = "simHD_BT_1_0.4_0.rda")
p10 <- sapply(c(1:length(result)), function(x)result[[x]]$p.value)
r10 <- mean(p10 < 0.05)
load(file = "simHD_BT_1_0.4_1.rda")
p11 <- sapply(c(1:length(result)), function(x)result[[x]]$p.value)
r11 <- mean(p11 < 0.05)
load(file = "simHD_BT_1_0.4_2.rda")
p12 <- sapply(c(1:length(result)), function(x)result[[x]]$p.value)
r12 <- mean(p12 < 0.05)

load(file = "simHD_BT_2_0.4_0.rda")
p20 <- sapply(c(1:length(result)), function(x)result[[x]]$p.value)
r20 <- mean(p20 < 0.05)
load(file = "simHD_BT_2_0.4_1.rda")
p21 <- sapply(c(1:length(result)), function(x)result[[x]]$p.value)
r21 <- mean(p21 < 0.05)
load(file = "simHD_BT_2_0.4_2.rda")
p22 <- sapply(c(1:length(result)), function(x)result[[x]]$p.value)
r22 <- mean(p22 < 0.05)


load(file = "simHD_BT_3_0.4_0.rda")
p30 <- sapply(c(1:length(result)), function(x)result[[x]]$pmean)
r30 <- mean(p30 < 0.05)
load(file = "simHD_BT_3_0.4_1.rda")
p31 <- sapply(c(1:length(result)), function(x)result[[x]]$pmean)
r31 <- mean(p31 < 0.05)
load(file = "simHD_BT_3_0.4_2.rda")
p32 <- sapply(c(1:length(result)), function(x)result[[x]]$pmean)
r32 <- mean(p32 < 0.05)

load(file = "simHD_BT_4_0.4_0.rda")
p40 <- sapply(c(1:length(result)), function(x)result[[x]]$pmean)
r40 <- mean(p40 < 0.05)
load(file = "simHD_BT_4_0.4_1.rda")
p41 <- sapply(c(1:length(result)), function(x)result[[x]]$pmean)
r41 <- mean(p41 < 0.05)
load(file = "simHD_BT_4_0.4_2.rda")
p42 <- sapply(c(1:length(result)), function(x)result[[x]]$pmean)
r42 <- mean(p42 < 0.05)

res <- data.frame(r1 = c(r10, r30, r20, r40),
           r2 = c(r11, r31, r21, r41),
           r3 = c(r12, r32, r22, r42))
rownames(res) = c("setting1_dc", "setting1_n",
                  "setting2_dc", "setting2_n")



write.csv(res, file = "result.csv")

