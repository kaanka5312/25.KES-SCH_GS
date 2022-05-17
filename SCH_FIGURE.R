###Global Signal Presented================== 
library(tidyverse);library(ggpubr);library(rstatix)
load("/media/kaan/Acer/SCH_GS/RDATA/NETW_GSCORR_RVAL_1.Rdata")

#######################SEFL REGIONS COMPARISON##############################
test<-as.data.frame(t(Zrval[1:3,]))
test<-test[,c("IntNetw_cor","ExtNetw_cor","MentNetw_cor")]
colnames(test)<-c("1-Int","2-Ext","3-Ment")
test$group<-as.factor(c(rep("SCH",30),rep("cont",26)))

id<-data.frame(id=c(1:nrow(test)))
test<-test %>% select(group,"1-Int","2-Ext","3-Ment") #Reordering to take mental inbetween
test<-test %>% mutate(.data=id)

test_id <- test %>%
  gather(key = "level", value = "score","1-Int","2-Ext","3-Ment") %>%
  convert_as_factor(id, group,level)

#Comparing SCH vs GS
stat.test <- test_id %>%
  group_by(level) %>%
  t_test(score ~ group) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance("p.adj")
stat.test


bxp <- ggboxplot(
  test_id, x = "level", y = "score",size = 1.5,
  color = "group",
  panel.labs = list(group=c("Control (n=26)","Schizophrenia (n=30)")))+
  #order=c("Int","Ext","Ment"))+
  ylab("GSCORR")+
  xlab("Layers")+
  ylim(0,2)+
  labs(title = "Self Regions")+
  theme(strip.text.x = element_text(
    size = 12, face = "bold"
  ),legend.position="top",
  legend.text=element_text(size=15),
  text=element_text(size=17))+
  scale_x_discrete(breaks=c("1-Int","2-Ext","3-Ment"),
                   labels=c("Interoceptive","Exteroceptive","Mental"))+
 scale_color_npg(name = "Groups", labels = c("Cont", "Sch"))

stat.test_GS <- stat.test %>%
  add_xy_position(x = "level", dodge = 0.8)


GS_level_Self<-bxp + stat_pvalue_manual(
  stat.test_GS,  label = "{p.adj.signif}", tip.length = 0,size=9,step.increase = 0.05
)

################################ WENGLER REGIONS COMPARISON#########################
test<-as.data.frame(t(Zrval[4:6,]))
test<-test[,c("Wengler_Netw_Aud","Wengler_Netw_Vis","Wengler_Netw_SMS")]
colnames(test)<-c("1-Aud","2-Vis","3-SMS")
test$group<-as.factor(c(rep("SCH",30),rep("cont",26)))

id<-data.frame(id=c(1:nrow(test)))
test<-test %>% select(group,"1-Aud","2-Vis","3-SMS") #Reordering to take mental inbetween
test<-test %>% mutate(.data=id)

test_id <- test %>%
  gather(key = "level", value = "score","1-Aud","2-Vis","3-SMS") %>%
  convert_as_factor(id, group,level)

#Comparing SCH vs GS
stat.test <- test_id %>%
  group_by(level) %>%
  t_test(score ~ group) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance("p.adj")
stat.test


bxp <- ggboxplot(
  test_id, x = "level", y = "score",size = 1.5,
  color = "group",
  panel.labs = list(group=c("Control (n=26)","Schizophrenia (n=30)")))+
  #order=c("Int","Ext","Ment"))+
  ylab("GSCORR")+
  xlab("Layers")+
  ylim(0,2)+
  labs(title = "Wengler Regions")+
  theme(strip.text.x = element_text(
    size = 12, face = "bold"
  ),legend.position="top",
  legend.text=element_text(size=15),
  text=element_text(size=17))+
  scale_x_discrete(breaks=c("1-Aud","2-Vis","3-SMS"),
                   labels=c("Auditory","Visual","Somatosensory"))+
  scale_color_npg(name = "Groups", labels = c("Cont", "Sch"))

stat.test_GS <- stat.test %>%
  add_xy_position(x = "level", dodge = 0.8)


GS_level_Weng<-bxp + stat_pvalue_manual(
  stat.test_GS,  label = "{p.adj.signif}", tip.length = 0,size=9,step.increase = 0.05
)

GroupComp<-ggarrange(
  GS_level_Self,GS_level_Weng,
  ncol=2,nrow = 1,
  align = "h",
  labels = "AUTO",
  font.label = list(size=20),
  heights = c(0.50, 0.50)
)

ggsave(GroupComp,filename="figure_1",device = "png",
       path = "/media/kaan/Acer/SCH_GS/RDATA/",width = 800,height = 800, dpi=72,units = c("px"))

############################################ ANOVA #############################
########SELF#####
#Control 
test_id %>% filter(group=="cont"
)%>%group_by(level
)%>%get_summary_stats(score, type = "mean_sd")

res.aov <- anova_test(data = test_id %>% filter(group=="cont"), formula = score~level)
get_anova_table(res.aov)



stat.test<-aov(score ~ level,data = test_id %>% filter(group=="cont")) %>% tukey_hsd()
stat.test<-stat.test %>%
  add_xy_position(formula=score ~ level,data=test_id %>% filter(group=="cont"),
                  x = "level", dodge = 0.8)


Cont<-ggboxplot(
  test_id %>% filter(group=="cont"),
  x = "level",
  y = "score",
  add="jitter",
  size = 1.5,
  color = "level",
  panel.labs = list(group=c("Control (n=26)","Schizophrenia (n=30)")))+
  #order=c("Int","Ext","Ment"))+
  ylab("GSCORR")+
  xlab("Layers")+
  ylim(0,2)+
  labs(title = "Control",
       subtitle = get_test_label(res.aov, detailed = TRUE),)+
  theme(strip.text.x = element_text(
    size = 12, face = "bold"
  ),legend.position="none",
  text=element_text(size=15))+
  scale_x_discrete(breaks=c("1-Int","2-Ext","3-Ment"),
                   labels=c("Interoceptive","Exteroceptive","Mental"))+
  scale_color_jco(name = "Groups", labels = c("Cont", "Sch"))+
  stat_pvalue_manual(
    stat.test, xmin = "group1",xmax="group2",y.position = 1.5,
    label = "{p.adj.signif}", tip.length = 0.02,size=7,step.increase = 0.1
  )
  
# SCH
res.aov <- anova_test(data = test_id %>% filter(group=="SCH"), formula = score~level)
get_anova_table(res.aov)



stat.test<-aov(score ~ level,data = test_id %>% filter(group=="SCH")) %>% tukey_hsd()
stat.test<-stat.test %>%
  add_xy_position(formula=score ~ level,data=test_id %>% filter(group=="SCH"),
                  x = "level", dodge = 0.8)


SCH<-ggboxplot(
  test_id %>% filter(group=="SCH"),
  x = "level",
  y = "score",
  add="jitter",
  size = 1.5,
  color = "level")+
  ylab("GSCORR")+
  xlab("Layers")+
  ylim(0,2)+
  labs(title = "Schizophrenia",
       subtitle = get_test_label(res.aov, detailed = TRUE),)+
  theme(strip.text.x = element_text(
    size = 12, face = "bold"
  ),legend.position="none",
  text=element_text(size=15))+
  scale_x_discrete(breaks=c("1-Int","2-Ext","3-Ment"),
                   labels=c("Interoceptive","Exteroceptive","Mental"))+
  scale_color_jco(name = "Groups", labels = c("Cont", "Sch"))+
  stat_pvalue_manual(
    stat.test, xmin = "group1",xmax="group2",y.position = 1.5,
    label = "{p.adj.signif}", tip.length = 0.02,size=7,step.increase = 0.1
  )

Tukey_Sef<-ggarrange(
  Cont,SCH,
  ncol=2,nrow = 1,
  align = "h",
  labels = c("A",""),
  font.label = list(size=20),
  heights = c(0.50, 0.50)
)

Self_Tukey<-annotate_figure(Tukey_Sef,
                top=text_grob("Self",face="bold",size=20))
#######WENGLER######

test_id %>% filter(group=="cont"
)%>%group_by(level
)%>%get_summary_stats(score, type = "mean_sd")

res.aov <- anova_test(data = test_id %>% filter(group=="cont"), formula = score~level)
get_anova_table(res.aov)



stat.test<-aov(score ~ level,data = test_id %>% filter(group=="cont")) %>% tukey_hsd()
stat.test<-stat.test %>%
  add_xy_position(formula=score ~ level,data=test_id %>% filter(group=="cont"),
                  x = "level", dodge = 0.8)


Cont<-ggboxplot(
  test_id %>% filter(group=="cont"),
  x = "level",
  y = "score",
  add="jitter",
  size = 1.5,
  color = "level")+
  ylab("GSCORR")+
  xlab("Layers")+
  ylim(0,2)+
  labs(title = "Control",
       subtitle = get_test_label(res.aov, detailed = TRUE),)+
  theme(strip.text.x = element_text(
    size = 12, face = "bold"
  ),legend.position="none",
  text=element_text(size=20))+
  scale_x_discrete(breaks=c("1-Aud","2-Vis","3-SMS"),
                   labels=c("Auditory","Visual","Somatosensory"))+
  scale_color_jco(name = "Groups", labels = c("Cont", "Sch"))+
  stat_pvalue_manual(
    stat.test, xmin = "group1",xmax="group2",y.position = 1.5,
    label = "{p.adj.signif}", tip.length = 0.02,size=7,step.increase = 0.1
  )

# SCH
res.aov <- anova_test(data = test_id %>% filter(group=="SCH"), formula = score~level)
get_anova_table(res.aov)



stat.test<-aov(score ~ level,data = test_id %>% filter(group=="SCH")) %>% tukey_hsd()
stat.test<-stat.test %>%
  add_xy_position(formula=score ~ level,data=test_id %>% filter(group=="SCH"),
                  x = "level", dodge = 0.8)


SCH<-ggboxplot(
  test_id %>% filter(group=="SCH"),
  x = "level",
  y = "score",
  add="jitter",
  size = 1.5,
  color = "level")+
  ylab("GSCORR")+
  xlab("Layers")+
  ylim(0,2)+
  labs(title = "Schizophrenia",
       subtitle = get_test_label(res.aov, detailed = TRUE),)+
  theme(strip.text.x = element_text(
    size = 12, face = "bold"
  ),legend.position="none",
  text=element_text(size=20))+
  scale_x_discrete(breaks=c("1-Aud","2-Vis","3-SMS"),
                   labels=c("Auditory","Visual","Somatosensory"))+
  scale_color_jco(name = "Groups", labels = c("Cont", "Sch"))+
  stat_pvalue_manual(
    stat.test, xmin = "group1",xmax="group2",y.position = 1.5,
    label = "{p.adj.signif}", tip.length = 0.02,size=7,step.increase = 0.1
  )

Tukey_Weng<-ggarrange(
  Cont,SCH,
  ncol=2,nrow = 1,
  align = "h",
  labels = c("B",""),
  font.label = list(size=20),
  heights = c(0.50, 0.50)
)

Self_Weng<-annotate_figure(Tukey_Weng,
                            top=text_grob("Wengler",face="bold",size=20))
       
#Combining two Anova
final<-ggarrange(
  Self_Tukey,Self_Weng,
  ncol=1,nrow = 2,
  align = "h",
  font.label = list(size=20),
  heights = c(0.50, 0.50)
)         
                

png(file="/media/kaan/Acer/SCH_GS/RDATA/figure_2.png",
    width=800, height=800)
final
dev.off()
