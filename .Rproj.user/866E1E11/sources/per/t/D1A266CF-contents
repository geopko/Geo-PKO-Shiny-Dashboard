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

map_df %>% filter(mission=="MINURSO") -> test

test %>% tibble::rowid_to_column("ID") %>% 
  select(ID, location, no.troops, no.tcc, rpf:uav, other.type, -rpf.no,
         -inf.no, -fpu.no, -res.no, -fp.no) %>% 
  mutate(across(everything(), as.character)) %>% 
  pivot_longer(5:23, names_to="trooptypes", values_to="binary") %>% 
  group_by(ID, location, no.troops, no.tcc, trooptypes) %>% 
  mutate(dc = sum(as.numeric(binary), na.rm=TRUE))  %>% 
  filter(bin)

  # mutate(trooptypes=case_when(trooptypes == "sf" ~ "Special Forces",
  #                             trooptypes == "inf" ~ "Infantry",
  #                             trooptypes == "he.sup" ~ "Helicopter Support",
  #                             trooptypes == "avia" ~ "Aviation",
  #                             trooptypes == "mp" ~ "Military Police",
  #                             trooptypes == "uav" ~ "Unmanned Aerial Vehicles",
  #                             trooptypes == "recon" ~ "Reconnaissance",
  #                             trooptypes == "maint" ~ "Maintenance",
  #                             trooptypes == "med" ~ "Medical",
  #                             trooptypes == "eng" ~ "Engineer",
  #                             trooptypes == "fpu" ~ "Formed Police Unit",
  #                             trooptypes == "fp" ~ "Force Protection",
  #                             trooptypes == "riv" ~ "Riverine",
  #                             trooptypes == "sig" ~ "Signal",
  #                             trooptypes == "trans" ~ "Transport",
  #                             trooptypes == "other.type" ~ "Others",
  #                             trooptypes == "eng" ~ "Engineer",
  #                             TRUE ~ as.character(trooptypes)))
  
  group_by(ID, location, no.troops, no.tcc) %>% 
  summarize(Troop.Compo = str_c(trooptypes, collapse=", ")) %>% ungroup() %>% 
  mutate(no.tcc=ifelse(is.na(no.tcc), "Unknown", no.tcc)) %>% 
  select(-ID)

test %>% tibble::rowid_to_column("ID") %>% 
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
