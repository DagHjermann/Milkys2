t_specimens %>%
  mutate(
    YEAR = year(DATE_CAUGHT),
    MONTH = month(DATE_CAUGHT),
    MYEAR = case_when(
      MONTH >= 4 ~ YEAR,
      MONTH < 4 ~ YEAR-1)) %>%
  left_join(t_samples_specimens, by = join_by(SPECIMEN_ID)) %>%
  left_join(t_samples, by = join_by(SAMPLE_ID)) %>% 
  left_join(t_tissue, by = join_by(TISSUE_ID)) %>% 
  left_join(t_lims_id, by = c("SAMPLE_ID" = "BIOTA_SAMPLE_ID")) %>% 
  left_join(t_measurements, by = join_by(SAMPLE_ID)) %>%
  left_join(t_methods, by = join_by(METHOD_ID)) %>%
  left_join(t_basis, by = join_by(BASIS_ID)) 

# This lists the 5 stations used by this project. OK!
t_project_stations %>%
  filter(PROJECT_ID %in% 12820) %>% collect()

# This lists the 5 projects using this station. OK!
t_project_stations %>%
  filter(STATION_ID %in% 50588) %>% collect()

# This does not only fetch the 15 (5 stations x 3) specimens covered by this project,
# but all specimens covered by the 5 stations (whatever project).
# But at least each specimen is only listed once
t_project_stations %>%
  filter(PROJECT_ID %in% 12820) %>%
  left_join(t_specimens) %>%
  count(STATION_CODE, STATION_ID) %>%
  arrange(STATION_CODE)

# ... same, but easer to see what's going on
t_project_stations %>%
  filter(PROJECT_ID %in% 12820) %>%
  left_join(t_specimens) %>%
  xtabs(~STATION_CODE + DATE_CAUGHT, .)

# This lists each specimen 3 times (onbce for each project)
t_project_stations %>%
  filter(PROJECT_ID %in% proj_id) %>%
  left_join(t_specimens) %>%
  xtabs(~STATION_CODE + DATE_CAUGHT, .)

# This lists each specimen only once, but we have to "collect" 
# in order to 
t_project_stations %>%
  filter(PROJECT_ID %in% proj_id) %>%
  left_join(t_specimens) %>%
  collect() %>%
  group_by(STATION_ID, STATION_CODE, SPECIMEN_ID, DATE_CAUGHT) %>%
  summarize(PROJECT_IDs = paste(PROJECT_ID, collapse = ",")) %>%
  xtabs(~STATION_CODE + DATE_CAUGHT, .)

# Using SQL only
DBI::dbGetQuery(con, 
                paste("select",
                "STATION_ID, listagg(PROJECT_ID, ',') within group (order by STATION_ID) AS projects", 
                "from NIVADATABASE.PROJECTS_STATIONS", "where STATION_ID = 50588", 
                "group by STATION_ID"))

# Using dbplyr with sql()
t_stations  <- tbl(con, in_schema("NIVADATABASE", "PROJECTS_STATIONS")) %>% 
  # add O_NUMBER column to data
  left_join(
    tbl(con, in_schema("NIVADATABASE", "PROJECTS_O_NUMBERS")) %>% select(PROJECT_ID, O_NUMBER),
    by = join_by(PROJECT_ID)
  ) %>%
  # summarize project information  
  group_by(STATION_ID) %>% 
  summarize(
    STATION_CODEs = sql("listagg(unique(STATION_CODE), ',') within group (order by PROJECT_ID)"),
    O_NUMBERs = sql("listagg(unique(O_NUMBER), ',') within group (order by PROJECT_ID)"),
    PROJECT_IDs = sql("listagg(PROJECT_ID, ',') within group (order by PROJECT_ID)")) %>%
  # add coordinate columns
  left_join(
    tbl(con, in_schema("NIVADATABASE", "STATIONS")) %>% select(STATION_ID, GEOM_REF_ID),
    by = join_by(STATION_ID)) %>% 
  left_join(
    tbl(con, in_schema("NIVA_GEOMETRY", "SAMPLE_POINTS")) %>% select(SAMPLE_POINT_ID, LONGITUDE, LATITUDE),
    by = join_by(GEOM_REF_ID == SAMPLE_POINT_ID))

# works
t_stations %>%
  filter(STATION_ID == 50588)
# works
t_stations %>%
  filter(STATION_CODEs == 'G1')
t_stations %>%
  filter(STATION_CODEs == 'G1') %>% show_query()
tbl(con, in_schema("NIVADATABASE", "PROJECTS_STATIONS")) %>% 
  filter(STATION_CODE == 'G1') 
tbl(con, in_schema("NIVADATABASE", "PROJECTS_STATIONS")) %>% 
  filter(sql("STATION_CODE = 'G1'")) 
# does not work
t_stations %>%
  filter(sql("O_NUMBERs like '%240237%'"))


# Concusion: split project info form station 

# Define projects_stations  
t_projects_stations  <- tbl(con, in_schema("NIVADATABASE", "PROJECTS_STATIONS")) %>% 
  # add O_NUMBER column to data
  left_join(
    tbl(con, in_schema("NIVADATABASE", "PROJECTS_O_NUMBERS")) %>% select(PROJECT_ID, O_NUMBER),
    by = join_by(PROJECT_ID)
  )

# projects_station
# works
t_projects_stations %>% 
  filter(PROJECT_ID == 12820)
# works (note: text)
t_projects_stations %>% 
  filter(O_NUMBER == '240237')
# works
t_projects_stations %>% 
  filter(STATION_CODE == 'G1')
t_projects_stations %>% 
  filter(sql("STATION_CODE like '%G1%'"))

# Step 1: select by projects_station info (if given)
t_projects_stations_selected <- t_projects_stations %>% 
  # if included: project search term 
  filter(STATION_NAME == 'Sandvika')

# Define stations  
t_stations  <- t_projects_stations_selected %>% 
  # summarize project information  
  group_by(STATION_ID) %>% 
  summarize(
    STATION_CODEs = sql("listagg(unique(STATION_CODE), ',') within group (order by PROJECT_ID)"),
    O_NUMBERs = sql("listagg(unique(O_NUMBER), ',') within group (order by PROJECT_ID)"),
    PROJECT_IDs = sql("listagg(PROJECT_ID, ',') within group (order by PROJECT_ID)")) %>%
  # add coordinate columns
  left_join(
    tbl(con, in_schema("NIVADATABASE", "STATIONS")) %>% select(STATION_ID, GEOM_REF_ID),
    by = join_by(STATION_ID)) %>% 
  left_join(
    tbl(con, in_schema("NIVA_GEOMETRY", "SAMPLE_POINTS")) %>% select(SAMPLE_POINT_ID, LONGITUDE, LATITUDE),
    by = join_by(GEOM_REF_ID == SAMPLE_POINT_ID))

# Step 2: filter by station info (if given)
t_stations %>%
  filter(STATION_ID == 50588)
t_stations %>%
  filter(LONGITUDE > 6.05 & LONGITUDE < 6.07 & LATITUDE > 61.1 & LATITUDE < 61.3)

# Define specimens
t_specimens <- tbl(connection, in_schema("NIVADATABASE", "BIOTA_SAMPLES")) %>%
  select(SAMPLE_ID, TISSUE_ID, SAMPLE_NO, REPNO, REMARK) %>%
  rename(REMARK_sample = REMARK) %>%
  left_join(
    tbl(connection, in_schema("NIVADATABASE", "TAXONOMY_CODES")) %>%
      select(TAXONOMY_CODE_ID, CODE, NIVA_TAXON_ID)
  ) %>%
  left_join(
    tbl(connection, in_schema("NIVADATABASE", "TAXONOMY")) %>%
      select(NIVA_TAXON_ID, LATIN_NAME)
  )

# drop STATION_ID, TAXONOMY_CODE_ID from samples
t_samples <<- tbl(connection, in_schema("NIVADATABASE", "BIOTA_SAMPLES")) %>%
  select(SAMPLE_ID, TISSUE_ID, SAMPLE_NO, REPNO, REMARK) %>%
  rename(REMARK_sample = REMARK)
t_samples_specimens <<- tbl(connection, in_schema("NIVADATABASE", "BIOTA_SAMPLES_SPECIMENS")) %>%
  select(SPECIMEN_ID, SAMPLE_ID)
t_specimens <<- tbl(connection, in_schema("NIVADATABASE", "BIOTA_SINGLE_SPECIMENS")) %>%
  select(STATION_ID, SPECIMEN_ID, SPECI



tbl(con, in_schema("NIVADATABASE", "PROJECTS_STATIONS")) %>% 
  # summarize project information  
  group_by(STATION_ID) %>% 
  summarize(
    STATION_CODEs = sql("listagg(unique(STATION_CODE), ',') within group (order by PROJECT_ID)"),
    PROJECT_IDs = sql("listagg(PROJECT_ID, ',') within group (order by PROJECT_ID)"))  %>% 
  filter(STATION_CODEs == 'G1') %>% show_query()

tbl(con, in_schema("NIVADATABASE", "PROJECTS_STATIONS")) %>% 
  # summarize project information  
  group_by(STATION_ID) %>% 
  summarize(
    STATION_CODEs = sql("listagg(unique(STATION_CODE), ',') within group (order by PROJECT_ID)"),
    PROJECT_IDs = sql("listagg(PROJECT_ID, ',') within group (order by PROJECT_ID)"))  %>% 
  filter(sql("'STATION_CODEs' = 'G1'")) 


# works
t_stations %>%
  filter(sql("'STATION_CODEs' = 'G1'"))
t_stations %>%
  filter(sqlSTATION_CODEs == 'G1')

  
  t_stations <<- tbl(connection, in_schema("NIVADATABASE", "STATIONS")) %>%
  select(STATION_ID, GEOM_REF_ID)
t_coordinates <<- tbl(connection, in_schema("NIVA_GEOMETRY", "SAMPLE_POINTS")) %>%
  select(SAMPLE_POINT_ID, LONGITUDE, LATITUDE)

  
  


tbl(con, in_schema("NIVADATABASE", "PROJECTS_STATIONS")) %>% 
  filter(STATION_ID == 50588) %>%
  group_by(STATION_ID) %>% 
  summarize(mean_id = sql("listagg(PROJECT_ID, ',') within group (order by STATION_ID)")) 

  group_by(STATION_ID) %>% 
  select(sql("STATION_ID, listagg(PROJECT_ID, ',') within group (order by STATION_ID) AS projects")) %>%
  show_query()

SELECT country,
LISTAGG(person, ', ') WITHIN GROUP (
  ORDER BY person) "names"
FROM mytable
GROUP BY country;

t_project_stations %>%
  filter(PROJECT_ID %in% proj_id) %>%
  left_join(t_specimens) %>% collect() %>% 
  View("spec three project")

t_project_stations %>%
  filter(PROJECT_ID %in% 12820) %>%
  left_join(t_specimens) %>% collect() %>% 
  View("spec one project")


t_project_stations %>%
  filter(PROJECT_ID %in% proj_id) %>%
  left_join(t_specimens) %>%
  count(STATION_CODE, STATION_ID) %>%
  arrange(STATION_CODE)

t_specimens %>%
  left_join(t_project_stations) %>%
  filter(PROJECT_ID %in% proj_id) %>%
  collect() %>% View()
  xtabs(~STATION_ID + PROJECT_ID, .)
