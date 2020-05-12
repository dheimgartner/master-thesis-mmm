# Script description ----
# In order to facilitate the arrangement of curve plots we store each modeling decomposition
# as individual plot. This allows us to arrange plots similar to as described here:
# https://github.com/compstat-lmu/iml_methods_limitations/blob/master/02-5-ale-other.Rmd
# which is easier than with facet_grid or facet_wrap...
#
# Requires: basically the aggregated data from the results script.

# -----------------------------------------------

# Libraries ----
library(ggpubr)
library(gridExtra)

# -----------------------------------------------

# media.1 ----
gam.wfd <-
  ggplot(curves.media.1 %>% filter(modeling == "gam", decomposition == "wfd"), 
         aes(x = media.spend, y = value, group = interaction(id, modeling, decomposition)), col = "black") +
  geom_line(alpha = 0.07) +
  geom_line(aes(x = media.spend, y = m.curve), col = "green") +
  geom_line(aes(x = media.spend, y = ci.curve.upper), col = "green", linetype = "dashed") +
  geom_line(aes(x = media.spend, y = ci.curve.lower), col = "green", linetype = "dashed") +
  geom_line(aes(x = media.spend, y = true.curve), col = "red") +
  labs(x = "media spend", y = "ROI", title = "gam, wfd, media 1") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme_bw() +
  scale_x_continuous(labels = scales::scientific) +
  scale_y_continuous(labels = scales::scientific)

gam.ale <-
  ggplot(curves.media.1 %>% filter(modeling == "gam", decomposition == "ale"), 
         aes(x = media.spend, y = value, group = interaction(id, modeling, decomposition)), col = "black") +
  geom_line(alpha = 0.07) +
  geom_line(aes(x = media.spend, y = m.curve), col = "green") +
  geom_line(aes(x = media.spend, y = ci.curve.upper), col = "green", linetype = "dashed") +
  geom_line(aes(x = media.spend, y = ci.curve.lower), col = "green", linetype = "dashed") +
  geom_line(aes(x = media.spend, y = true.curve), col = "red") +
  labs(x = "media spend", y = "ROI", title = "gam, ale, media 1") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme_bw() +
  scale_x_continuous(labels = scales::scientific) +
  scale_y_continuous(labels = scales::scientific)

gam.shap <-
  ggplot(curves.media.1 %>% filter(modeling == "gam", decomposition == "shap"), 
         aes(x = media.spend, y = value, group = interaction(id, modeling, decomposition)), col = "black") +
  geom_line(alpha = 0.07) +
  geom_line(aes(x = media.spend, y = m.curve), col = "green") +
  geom_line(aes(x = media.spend, y = ci.curve.upper), col = "green", linetype = "dashed") +
  geom_line(aes(x = media.spend, y = ci.curve.lower), col = "green", linetype = "dashed") +
  geom_line(aes(x = media.spend, y = true.curve), col = "red") +
  labs(x = "media spend", y = "ROI", title = "gam, shap, media 1") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme_bw() +
  scale_x_continuous(labels = scales::scientific) +
  scale_y_continuous(labels = scales::scientific)

gam_2.wfd <-
  ggplot(curves.media.1 %>% filter(modeling == "gam_2", decomposition == "wfd"), 
         aes(x = media.spend, y = value, group = interaction(id, modeling, decomposition)), col = "black") +
  geom_line(alpha = 0.07) +
  geom_line(aes(x = media.spend, y = m.curve), col = "green") +
  geom_line(aes(x = media.spend, y = ci.curve.upper), col = "green", linetype = "dashed") +
  geom_line(aes(x = media.spend, y = ci.curve.lower), col = "green", linetype = "dashed") +
  geom_line(aes(x = media.spend, y = true.curve), col = "red") +
  labs(x = "media spend", y = "ROI", title = "gam_2, wfd, media 1") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme_bw() +
  scale_x_continuous(labels = scales::scientific) +
  scale_y_continuous(labels = scales::scientific)

gam_2.ale <-
  ggplot(curves.media.1 %>% filter(modeling == "gam_2", decomposition == "ale"), 
         aes(x = media.spend, y = value, group = interaction(id, modeling, decomposition)), col = "black") +
  geom_line(alpha = 0.07) +
  geom_line(aes(x = media.spend, y = m.curve), col = "green") +
  geom_line(aes(x = media.spend, y = ci.curve.upper), col = "green", linetype = "dashed") +
  geom_line(aes(x = media.spend, y = ci.curve.lower), col = "green", linetype = "dashed") +
  geom_line(aes(x = media.spend, y = true.curve), col = "red") +
  labs(x = "media spend", y = "ROI", title = "gam_2, ale, media 1") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme_bw() +
  scale_x_continuous(labels = scales::scientific) +
  scale_y_continuous(labels = scales::scientific)

gam_2.shap <-
  ggplot(curves.media.1 %>% filter(modeling == "gam_2", decomposition == "shap"), 
         aes(x = media.spend, y = value, group = interaction(id, modeling, decomposition)), col = "black") +
  geom_line(alpha = 0.07) +
  geom_line(aes(x = media.spend, y = m.curve), col = "green") +
  geom_line(aes(x = media.spend, y = ci.curve.upper), col = "green", linetype = "dashed") +
  geom_line(aes(x = media.spend, y = ci.curve.lower), col = "green", linetype = "dashed") +
  geom_line(aes(x = media.spend, y = true.curve), col = "red") +
  labs(x = "media spend", y = "ROI", title = "gam_2, shap, media 1") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme_bw() +
  scale_x_continuous(labels = scales::scientific) +
  scale_y_continuous(labels = scales::scientific)

ols_poly.wfd <-
  ggplot(curves.media.1 %>% filter(modeling == "ols_poly", decomposition == "wfd"), 
         aes(x = media.spend, y = value, group = interaction(id, modeling, decomposition)), col = "black") +
  geom_line(alpha = 0.07) +
  geom_line(aes(x = media.spend, y = m.curve), col = "green") +
  geom_line(aes(x = media.spend, y = ci.curve.upper), col = "green", linetype = "dashed") +
  geom_line(aes(x = media.spend, y = ci.curve.lower), col = "green", linetype = "dashed") +
  geom_line(aes(x = media.spend, y = true.curve), col = "red") +
  labs(x = "media spend", y = "ROI", title = "ols_poly, wfd, media 1") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme_bw() +
  scale_x_continuous(labels = scales::scientific) +
  scale_y_continuous(labels = scales::scientific)

ols_poly.ale <-
  ggplot(curves.media.1 %>% filter(modeling == "ols_poly", decomposition == "ale"), 
         aes(x = media.spend, y = value, group = interaction(id, modeling, decomposition)), col = "black") +
  geom_line(alpha = 0.07) +
  geom_line(aes(x = media.spend, y = m.curve), col = "green") +
  geom_line(aes(x = media.spend, y = ci.curve.upper), col = "green", linetype = "dashed") +
  geom_line(aes(x = media.spend, y = ci.curve.lower), col = "green", linetype = "dashed") +
  geom_line(aes(x = media.spend, y = true.curve), col = "red") +
  labs(x = "media spend", y = "ROI", title = "ols_poly, ale, media 1") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme_bw() +
  scale_x_continuous(labels = scales::scientific) +
  scale_y_continuous(labels = scales::scientific)

ols_poly.shap <-
  ggplot(curves.media.1 %>% filter(modeling == "ols_poly", decomposition == "shap"), 
         aes(x = media.spend, y = value, group = interaction(id, modeling, decomposition)), col = "black") +
  geom_line(alpha = 0.07) +
  geom_line(aes(x = media.spend, y = m.curve), col = "green") +
  geom_line(aes(x = media.spend, y = ci.curve.upper), col = "green", linetype = "dashed") +
  geom_line(aes(x = media.spend, y = ci.curve.lower), col = "green", linetype = "dashed") +
  geom_line(aes(x = media.spend, y = true.curve), col = "red") +
  labs(x = "media spend", y = "ROI", title = "ols_poly, shap, media 1") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme_bw() +
  scale_x_continuous(labels = scales::scientific) +
  scale_y_continuous(labels = scales::scientific)

ols_multi.ale <-
  ggplot(curves.media.1 %>% filter(modeling == "ols_multi", decomposition == "ale"), 
         aes(x = media.spend, y = value, group = interaction(id, modeling, decomposition)), col = "black") +
  geom_line(alpha = 0.07) +
  geom_line(aes(x = media.spend, y = m.curve), col = "green") +
  geom_line(aes(x = media.spend, y = ci.curve.upper), col = "green", linetype = "dashed") +
  geom_line(aes(x = media.spend, y = ci.curve.lower), col = "green", linetype = "dashed") +
  geom_line(aes(x = media.spend, y = true.curve), col = "red") +
  labs(x = "media spend", y = "ROI", title = "ols_multi, ale, media 1") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme_bw() +
  scale_x_continuous(labels = scales::scientific) +
  scale_y_continuous(labels = scales::scientific)

ols_multi.shap <-
  ggplot(curves.media.1 %>% filter(modeling == "ols_multi", decomposition == "shap"), 
         aes(x = media.spend, y = value, group = interaction(id, modeling, decomposition)), col = "black") +
  geom_line(alpha = 0.07) +
  geom_line(aes(x = media.spend, y = m.curve), col = "green") +
  geom_line(aes(x = media.spend, y = ci.curve.upper), col = "green", linetype = "dashed") +
  geom_line(aes(x = media.spend, y = ci.curve.lower), col = "green", linetype = "dashed") +
  geom_line(aes(x = media.spend, y = true.curve), col = "red") +
  labs(x = "media spend", y = "ROI", title = "ols_multi, shap, media 1") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme_bw() +
  scale_x_continuous(labels = scales::scientific) +
  scale_y_continuous(labels = scales::scientific)

svr_poly.wfd <-
  ggplot(curves.media.1 %>% filter(modeling == "svr_poly", decomposition == "wfd"), 
         aes(x = media.spend, y = value, group = interaction(id, modeling, decomposition)), col = "black") +
  geom_line(alpha = 0.07) +
  geom_line(aes(x = media.spend, y = m.curve), col = "green") +
  geom_line(aes(x = media.spend, y = ci.curve.upper), col = "green", linetype = "dashed") +
  geom_line(aes(x = media.spend, y = ci.curve.lower), col = "green", linetype = "dashed") +
  geom_line(aes(x = media.spend, y = true.curve), col = "red") +
  labs(x = "media spend", y = "ROI", title = "svr_poly, wfd, media 1") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme_bw() +
  scale_x_continuous(labels = scales::scientific) +
  scale_y_continuous(labels = scales::scientific)

svr_poly.ale <-
  ggplot(curves.media.1 %>% filter(modeling == "svr_poly", decomposition == "ale"), 
         aes(x = media.spend, y = value, group = interaction(id, modeling, decomposition)), col = "black") +
  geom_line(alpha = 0.07) +
  geom_line(aes(x = media.spend, y = m.curve), col = "green") +
  geom_line(aes(x = media.spend, y = ci.curve.upper), col = "green", linetype = "dashed") +
  geom_line(aes(x = media.spend, y = ci.curve.lower), col = "green", linetype = "dashed") +
  geom_line(aes(x = media.spend, y = true.curve), col = "red") +
  labs(x = "media spend", y = "ROI", title = "svr_poly, ale, media 1") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme_bw() +
  scale_x_continuous(labels = scales::scientific) +
  scale_y_continuous(labels = scales::scientific)

svr_poly.shap <-
  ggplot(curves.media.1 %>% filter(modeling == "svr_poly", decomposition == "shap"), 
         aes(x = media.spend, y = value, group = interaction(id, modeling, decomposition)), col = "black") +
  geom_line(alpha = 0.07) +
  geom_line(aes(x = media.spend, y = m.curve), col = "green") +
  geom_line(aes(x = media.spend, y = ci.curve.upper), col = "green", linetype = "dashed") +
  geom_line(aes(x = media.spend, y = ci.curve.lower), col = "green", linetype = "dashed") +
  geom_line(aes(x = media.spend, y = true.curve), col = "red") +
  labs(x = "media spend", y = "ROI", title = "svr_poly, shap, media 1") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme_bw() +
  scale_x_continuous(labels = scales::scientific) +
  scale_y_continuous(labels = scales::scientific)

svr_multi.ale <-
  ggplot(curves.media.1 %>% filter(modeling == "svr_multi", decomposition == "ale"), 
         aes(x = media.spend, y = value, group = interaction(id, modeling, decomposition)), col = "black") +
  geom_line(alpha = 0.07) +
  geom_line(aes(x = media.spend, y = m.curve), col = "green") +
  geom_line(aes(x = media.spend, y = ci.curve.upper), col = "green", linetype = "dashed") +
  geom_line(aes(x = media.spend, y = ci.curve.lower), col = "green", linetype = "dashed") +
  geom_line(aes(x = media.spend, y = true.curve), col = "red") +
  labs(x = "media spend", y = "ROI", title = "svr_multi, ale, media 1") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme_bw() +
  scale_x_continuous(labels = scales::scientific) +
  scale_y_continuous(labels = scales::scientific)

svr_multi.shap <-
  ggplot(curves.media.1 %>% filter(modeling == "svr_multi", decomposition == "shap"), 
         aes(x = media.spend, y = value, group = interaction(id, modeling, decomposition)), col = "black") +
  geom_line(alpha = 0.07) +
  geom_line(aes(x = media.spend, y = m.curve), col = "green") +
  geom_line(aes(x = media.spend, y = ci.curve.upper), col = "green", linetype = "dashed") +
  geom_line(aes(x = media.spend, y = ci.curve.lower), col = "green", linetype = "dashed") +
  geom_line(aes(x = media.spend, y = true.curve), col = "red") +
  labs(x = "media spend", y = "ROI", title = "svr_multi, shap, media 1") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme_bw() +
  scale_x_continuous(labels = scales::scientific) +
  scale_y_continuous(labels = scales::scientific)

tvem.wfd <-
  ggplot(curves.media.1 %>% filter(modeling == "tvem", decomposition == "wfd"), 
         aes(x = media.spend, y = value, group = interaction(id, modeling, decomposition)), col = "black") +
  geom_line(alpha = 0.07) +
  geom_line(aes(x = media.spend, y = m.curve), col = "green") +
  geom_line(aes(x = media.spend, y = ci.curve.upper), col = "green", linetype = "dashed") +
  geom_line(aes(x = media.spend, y = ci.curve.lower), col = "green", linetype = "dashed") +
  geom_line(aes(x = media.spend, y = true.curve), col = "red") +
  labs(x = "media spend", y = "ROI", title = "tvem, wfd, media 1") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme_bw() +
  scale_x_continuous(labels = scales::scientific) +
  scale_y_continuous(labels = scales::scientific)

# -----------------------------------------------

# media.2 ----
gam.wfd.2 <-
  ggplot(curves.media.2 %>% filter(modeling == "gam", decomposition == "wfd"), 
         aes(x = media.spend, y = value, group = interaction(id, modeling, decomposition)), col = "black") +
  geom_line(alpha = 0.07) +
  geom_line(aes(x = media.spend, y = m.curve), col = "green") +
  geom_line(aes(x = media.spend, y = ci.curve.upper), col = "green", linetype = "dashed") +
  geom_line(aes(x = media.spend, y = ci.curve.lower), col = "green", linetype = "dashed") +
  geom_line(aes(x = media.spend, y = true.curve), col = "red") +
  labs(x = "media spend", y = "ROI", title = "gam, wfd, media 2") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme_bw() +
  scale_x_continuous(labels = scales::scientific) +
  scale_y_continuous(labels = scales::scientific)

gam.ale.2 <-
  ggplot(curves.media.2 %>% filter(modeling == "gam", decomposition == "ale"), 
         aes(x = media.spend, y = value, group = interaction(id, modeling, decomposition)), col = "black") +
  geom_line(alpha = 0.07) +
  geom_line(aes(x = media.spend, y = m.curve), col = "green") +
  geom_line(aes(x = media.spend, y = ci.curve.upper), col = "green", linetype = "dashed") +
  geom_line(aes(x = media.spend, y = ci.curve.lower), col = "green", linetype = "dashed") +
  geom_line(aes(x = media.spend, y = true.curve), col = "red") +
  labs(x = "media spend", y = "ROI", title = "gam, ale, media 2") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme_bw() +
  scale_x_continuous(labels = scales::scientific) +
  scale_y_continuous(labels = scales::scientific)

gam.shap.2 <-
  ggplot(curves.media.2 %>% filter(modeling == "gam", decomposition == "shap"), 
         aes(x = media.spend, y = value, group = interaction(id, modeling, decomposition)), col = "black") +
  geom_line(alpha = 0.07) +
  geom_line(aes(x = media.spend, y = m.curve), col = "green") +
  geom_line(aes(x = media.spend, y = ci.curve.upper), col = "green", linetype = "dashed") +
  geom_line(aes(x = media.spend, y = ci.curve.lower), col = "green", linetype = "dashed") +
  geom_line(aes(x = media.spend, y = true.curve), col = "red") +
  labs(x = "media spend", y = "ROI", title = "gam, shap, media 2") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme_bw() +
  scale_x_continuous(labels = scales::scientific) +
  scale_y_continuous(labels = scales::scientific)

gam_2.wfd.2 <-
  ggplot(curves.media.2 %>% filter(modeling == "gam_2", decomposition == "wfd"), 
         aes(x = media.spend, y = value, group = interaction(id, modeling, decomposition)), col = "black") +
  geom_line(alpha = 0.07) +
  geom_line(aes(x = media.spend, y = m.curve), col = "green") +
  geom_line(aes(x = media.spend, y = ci.curve.upper), col = "green", linetype = "dashed") +
  geom_line(aes(x = media.spend, y = ci.curve.lower), col = "green", linetype = "dashed") +
  geom_line(aes(x = media.spend, y = true.curve), col = "red") +
  labs(x = "media spend", y = "ROI", title = "gam_2, wfd, media 2") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme_bw() +
  scale_x_continuous(labels = scales::scientific) +
  scale_y_continuous(labels = scales::scientific)

gam_2.ale.2 <-
  ggplot(curves.media.2 %>% filter(modeling == "gam_2", decomposition == "ale"), 
         aes(x = media.spend, y = value, group = interaction(id, modeling, decomposition)), col = "black") +
  geom_line(alpha = 0.07) +
  geom_line(aes(x = media.spend, y = m.curve), col = "green") +
  geom_line(aes(x = media.spend, y = ci.curve.upper), col = "green", linetype = "dashed") +
  geom_line(aes(x = media.spend, y = ci.curve.lower), col = "green", linetype = "dashed") +
  geom_line(aes(x = media.spend, y = true.curve), col = "red") +
  labs(x = "media spend", y = "ROI", title = "gam_2, ale, media 2") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme_bw() +
  scale_x_continuous(labels = scales::scientific) +
  scale_y_continuous(labels = scales::scientific)

gam_2.shap.2 <-
  ggplot(curves.media.2 %>% filter(modeling == "gam_2", decomposition == "shap"), 
         aes(x = media.spend, y = value, group = interaction(id, modeling, decomposition)), col = "black") +
  geom_line(alpha = 0.07) +
  geom_line(aes(x = media.spend, y = m.curve), col = "green") +
  geom_line(aes(x = media.spend, y = ci.curve.upper), col = "green", linetype = "dashed") +
  geom_line(aes(x = media.spend, y = ci.curve.lower), col = "green", linetype = "dashed") +
  geom_line(aes(x = media.spend, y = true.curve), col = "red") +
  labs(x = "media spend", y = "ROI", title = "gam_2, shap, media 2") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme_bw() +
  scale_x_continuous(labels = scales::scientific) +
  scale_y_continuous(labels = scales::scientific)

ols_poly.wfd.2 <-
  ggplot(curves.media.2 %>% filter(modeling == "ols_poly", decomposition == "wfd"), 
         aes(x = media.spend, y = value, group = interaction(id, modeling, decomposition)), col = "black") +
  geom_line(alpha = 0.07) +
  geom_line(aes(x = media.spend, y = m.curve), col = "green") +
  geom_line(aes(x = media.spend, y = ci.curve.upper), col = "green", linetype = "dashed") +
  geom_line(aes(x = media.spend, y = ci.curve.lower), col = "green", linetype = "dashed") +
  geom_line(aes(x = media.spend, y = true.curve), col = "red") +
  labs(x = "media spend", y = "ROI", title = "ols_poly, wfd, media 2") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme_bw() +
  scale_x_continuous(labels = scales::scientific) +
  scale_y_continuous(labels = scales::scientific)

ols_poly.ale.2 <-
  ggplot(curves.media.2 %>% filter(modeling == "ols_poly", decomposition == "ale"), 
         aes(x = media.spend, y = value, group = interaction(id, modeling, decomposition)), col = "black") +
  geom_line(alpha = 0.07) +
  geom_line(aes(x = media.spend, y = m.curve), col = "green") +
  geom_line(aes(x = media.spend, y = ci.curve.upper), col = "green", linetype = "dashed") +
  geom_line(aes(x = media.spend, y = ci.curve.lower), col = "green", linetype = "dashed") +
  geom_line(aes(x = media.spend, y = true.curve), col = "red") +
  labs(x = "media spend", y = "ROI", title = "ols_poly, ale, media 2") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme_bw() +
  scale_x_continuous(labels = scales::scientific) +
  scale_y_continuous(labels = scales::scientific)

ols_poly.shap.2 <-
  ggplot(curves.media.2 %>% filter(modeling == "ols_poly", decomposition == "shap"), 
         aes(x = media.spend, y = value, group = interaction(id, modeling, decomposition)), col = "black") +
  geom_line(alpha = 0.07) +
  geom_line(aes(x = media.spend, y = m.curve), col = "green") +
  geom_line(aes(x = media.spend, y = ci.curve.upper), col = "green", linetype = "dashed") +
  geom_line(aes(x = media.spend, y = ci.curve.lower), col = "green", linetype = "dashed") +
  geom_line(aes(x = media.spend, y = true.curve), col = "red") +
  labs(x = "media spend", y = "ROI", title = "ols_poly, shap, media 2") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme_bw() +
  scale_x_continuous(labels = scales::scientific) +
  scale_y_continuous(labels = scales::scientific)

ols_multi.ale.2 <-
  ggplot(curves.media.2 %>% filter(modeling == "ols_multi", decomposition == "ale"), 
         aes(x = media.spend, y = value, group = interaction(id, modeling, decomposition)), col = "black") +
  geom_line(alpha = 0.07) +
  geom_line(aes(x = media.spend, y = m.curve), col = "green") +
  geom_line(aes(x = media.spend, y = ci.curve.upper), col = "green", linetype = "dashed") +
  geom_line(aes(x = media.spend, y = ci.curve.lower), col = "green", linetype = "dashed") +
  geom_line(aes(x = media.spend, y = true.curve), col = "red") +
  labs(x = "media spend", y = "ROI", title = "ols_multi, ale, media 2") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme_bw() +
  scale_x_continuous(labels = scales::scientific) +
  scale_y_continuous(labels = scales::scientific)

ols_multi.shap.2 <-
  ggplot(curves.media.2 %>% filter(modeling == "ols_multi", decomposition == "shap"), 
         aes(x = media.spend, y = value, group = interaction(id, modeling, decomposition)), col = "black") +
  geom_line(alpha = 0.07) +
  geom_line(aes(x = media.spend, y = m.curve), col = "green") +
  geom_line(aes(x = media.spend, y = ci.curve.upper), col = "green", linetype = "dashed") +
  geom_line(aes(x = media.spend, y = ci.curve.lower), col = "green", linetype = "dashed") +
  geom_line(aes(x = media.spend, y = true.curve), col = "red") +
  labs(x = "media spend", y = "ROI", title = "ols_multi, shap, media 2") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme_bw() +
  scale_x_continuous(labels = scales::scientific) +
  scale_y_continuous(labels = scales::scientific)

svr_poly.wfd.2 <-
  ggplot(curves.media.2 %>% filter(modeling == "svr_poly", decomposition == "wfd"), 
         aes(x = media.spend, y = value, group = interaction(id, modeling, decomposition)), col = "black") +
  geom_line(alpha = 0.07) +
  geom_line(aes(x = media.spend, y = m.curve), col = "green") +
  geom_line(aes(x = media.spend, y = ci.curve.upper), col = "green", linetype = "dashed") +
  geom_line(aes(x = media.spend, y = ci.curve.lower), col = "green", linetype = "dashed") +
  geom_line(aes(x = media.spend, y = true.curve), col = "red") +
  labs(x = "media spend", y = "ROI", title = "svr_poly, wfd, media 2") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme_bw() +
  scale_x_continuous(labels = scales::scientific) +
  scale_y_continuous(labels = scales::scientific)

svr_poly.ale.2 <-
  ggplot(curves.media.2 %>% filter(modeling == "svr_poly", decomposition == "ale"), 
         aes(x = media.spend, y = value, group = interaction(id, modeling, decomposition)), col = "black") +
  geom_line(alpha = 0.07) +
  geom_line(aes(x = media.spend, y = m.curve), col = "green") +
  geom_line(aes(x = media.spend, y = ci.curve.upper), col = "green", linetype = "dashed") +
  geom_line(aes(x = media.spend, y = ci.curve.lower), col = "green", linetype = "dashed") +
  geom_line(aes(x = media.spend, y = true.curve), col = "red") +
  labs(x = "media spend", y = "ROI", title = "svr_poly, ale, media 2") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme_bw() +
  scale_x_continuous(labels = scales::scientific) +
  scale_y_continuous(labels = scales::scientific)

svr_poly.shap.2 <-
  ggplot(curves.media.2 %>% filter(modeling == "svr_poly", decomposition == "shap"), 
         aes(x = media.spend, y = value, group = interaction(id, modeling, decomposition)), col = "black") +
  geom_line(alpha = 0.07) +
  geom_line(aes(x = media.spend, y = m.curve), col = "green") +
  geom_line(aes(x = media.spend, y = ci.curve.upper), col = "green", linetype = "dashed") +
  geom_line(aes(x = media.spend, y = ci.curve.lower), col = "green", linetype = "dashed") +
  geom_line(aes(x = media.spend, y = true.curve), col = "red") +
  labs(x = "media spend", y = "ROI", title = "svr_poly, shap, media 2") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme_bw() +
  scale_x_continuous(labels = scales::scientific) +
  scale_y_continuous(labels = scales::scientific)

svr_multi.ale.2 <-
  ggplot(curves.media.2 %>% filter(modeling == "svr_multi", decomposition == "ale"), 
         aes(x = media.spend, y = value, group = interaction(id, modeling, decomposition)), col = "black") +
  geom_line(alpha = 0.07) +
  geom_line(aes(x = media.spend, y = m.curve), col = "green") +
  geom_line(aes(x = media.spend, y = ci.curve.upper), col = "green", linetype = "dashed") +
  geom_line(aes(x = media.spend, y = ci.curve.lower), col = "green", linetype = "dashed") +
  geom_line(aes(x = media.spend, y = true.curve), col = "red") +
  labs(x = "media spend", y = "ROI", title = "svr_multi, ale, media 2") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme_bw() +
  scale_x_continuous(labels = scales::scientific) +
  scale_y_continuous(labels = scales::scientific)

svr_multi.shap.2 <-
  ggplot(curves.media.2 %>% filter(modeling == "svr_multi", decomposition == "shap"), 
         aes(x = media.spend, y = value, group = interaction(id, modeling, decomposition)), col = "black") +
  geom_line(alpha = 0.07) +
  geom_line(aes(x = media.spend, y = m.curve), col = "green") +
  geom_line(aes(x = media.spend, y = ci.curve.upper), col = "green", linetype = "dashed") +
  geom_line(aes(x = media.spend, y = ci.curve.lower), col = "green", linetype = "dashed") +
  geom_line(aes(x = media.spend, y = true.curve), col = "red") +
  labs(x = "media spend", y = "ROI", title = "svr_multi, shap, media 2") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme_bw() +
  scale_x_continuous(labels = scales::scientific) +
  scale_y_continuous(labels = scales::scientific)

tvem.wfd.2 <-
  ggplot(curves.media.2 %>% filter(modeling == "tvem", decomposition == "wfd"), 
         aes(x = media.spend, y = value, group = interaction(id, modeling, decomposition)), col = "black") +
  geom_line(alpha = 0.07) +
  geom_line(aes(x = media.spend, y = m.curve), col = "green") +
  geom_line(aes(x = media.spend, y = ci.curve.upper), col = "green", linetype = "dashed") +
  geom_line(aes(x = media.spend, y = ci.curve.lower), col = "green", linetype = "dashed") +
  geom_line(aes(x = media.spend, y = true.curve), col = "red") +
  labs(x = "media spend", y = "ROI", title = "tvem, wfd, media 2") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme_bw() +
  scale_x_continuous(labels = scales::scientific) +
  scale_y_continuous(labels = scales::scientific)

# -----------------------------------------------

# Arrange ----
# ggarrange(ggarrange(gam.wfd, gam.ale, gam.shap, ncol = 3),
#           ggarrange(gam_2.wfd, gam_2.ale, gam_2.shap, ncol = 3), 
#           ggarrange(ols_poly.wfd, ols_poly.ale, ols_poly.shap, ncol = 3),
#           ggarrange(NULL, ols_multi.ale, ols_multi.shap, ncol = 3),
#           ggarrange(NULL, svr_poly.wfd, svr_poly.ale, svr_poly.shap, ncol = 3),
#           ggarrange(svr_multi.ale, svr_multi.shap, ncol = 3),
#           ggarrange(tvem.wfd, NULL, NULL, ncol = 3), 
#           nrow = 7)

# ggarrange(gam.wfd, gam.shap, gam.ale, gam_2.wfd, gam_2.ale, gam_2.shap, ols_poly.wfd,
#           ols_poly.ale, ols_poly.shap, ols_multi.ale, ols_multi.shap, svr_poly.wfd,
#           svr_poly.ale, svr_poly.shap, svr_multi.ale, svr_multi.shap, tvem.wfd,
#           ncol = 6)

# ggarrange(ggarrange(gam.wfd, gam_2.wfd, ols_poly.wfd, NULL, svr_poly.wfd, NULL, tvem.wfd, ncol = 7),
#           ggarrange(gam.ale, gam_2.ale, ols_poly.ale, ols_multi.ale, svr_poly.ale, svr_multi.ale, NULL, ncol = 7), 
#           ggarrange(gam.shap, gam_2.shap, ols_poly.shap, ols_multi.shap, svr_poly.shap, svr_multi.shap, NULL , ncol = 7),
#           nrow = 3)
