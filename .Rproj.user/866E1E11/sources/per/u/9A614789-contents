bymap_df <- 
  tcc_df %>% filter(Mission=="UNIFIL", Source=="Map NO. 3000 Rev. 40") %>% 
    pivot_longer(c(nameoftcc_1:notroopspertcc_17), names_to=c(".value", "tcc_id"), names_sep="_") %>%
    filter(!is.na(nameoftcc)) %>%
    mutate_at(vars(notroopspertcc), as.numeric) %>% 
    select(-tcc_id) %>% 
    group_by(Source, Mission, Year, Month, Total.troops, nameoftcc)%>%
    summarise(total.tcc=sum(notroopspertcc)) %>% 
    add_count(Source, name="No.TCC") %>%
    mutate(total.tcc=ifelse(is.na(total.tcc), "size unknown", total.tcc), 
           overview=paste0(nameoftcc," (",total.tcc,")")) %>%
    group_by(Source, Mission, Year, Month, Total.troops, No.TCC) %>%
    summarise(details=str_c(overview, collapse=", ")) %>% 
    arrange(desc(Year))

test %>%  tibble::rowid_to_column("ID") %>% 
  select(ID, location, No.troops, No.TCC, RPF:UAV, Other.Type, -RPF_No,
         -Inf_No, -FPU_No, -RES_No, -FP_No) %>% 
  mutate(across(everything(), as.character)) %>% 
  pivot_longer(5:23, names_to="trooptypes", values_to="binary") %>% 
  filter(binary==1) %>% 
  mutate(trooptypes=case_when(trooptypes == "Other.Type" ~ "Others",
                              trooptypes == "SF" ~ "Special Forces", 
                              trooptypes == "Inf" ~ "Infantry",
                              TRUE ~ as.character(trooptypes))) %>% 
  group_by(ID, location, No.troops, No.TCC) %>% 
  summarize(Troop.Compo = str_c(trooptypes, collapse=", ")) %>% ungroup() %>% 
  mutate(No.TCC=ifelse(is.na(No.TCC), "Unknown", No.TCC)) %>% 
  select(-ID) -> testtable

map_df %>% tibble::rowid_to_column("ID") %>% 
  select(ID, Location, No.troops, No.TCC, RPF:UAV, Other.Type, -RPF_No,
         -Inf_No, -FPU_No, -RES_No, -FP_No) %>% 
  mutate_at(vars(`Inf`:`Other.Type`), as.numeric) %>% 
  rowwise(ID) %>% 
  mutate(typecheck_var=sum(c_across(`Inf`:`Other.Type`)))  -> test2


map_df2 %>% filter(Mission=="MINURSO") -> test

test %>% slice(1) %>%  pull(No.troops) %>%  `^`(1/3) -> sizen

testp <- ggplot()+ geom_point(data=test %>% slice(1),
                             aes(x=longitude, y=latitude, color=as.integer(No.TCC), size="Custom"),
                             shape=20, alpha = 0.8)+
  scale_size_manual(name="Size of deployment", values=c("Custom"=)
