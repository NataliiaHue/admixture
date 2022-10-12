# investigate the data

FEATURES_TO_IGNORE <- c(
  "GB024", "GB025", "GB065", "GB130", "GB193", "GB203",
  "TE001", "TE002", "TE009", "TE012", "TE014", "TE015", "TE016", "TE022", "TE025",
  "TE026", "TE028", "TE029", "TE033", "TE034", "TE036", "TE040", "TE041", "TE042",
  "TE043", "TE044", "TE045", "TE046", "TE047", "TE048", "TE049", "TE051", "TE055",
  "TE056", "TE057", "TE058", "TE060", "TE061", "TE062", "TE063", "TE064", "TE065",
  "TE067", "TE068", "TE069", "TE070", "TE071", "TE072", "TE073", "TE074", "TE076",
  "TE077", "TS081", "TS082", "TS083", "TS083", "TS084", "TS085"
)


# Read in coded values
# values <- read_csv("https://raw.githubusercontent.com/cldf-datasets/hueblerstability/main/cldf/values.csv")
values <- read_csv("./hueblerstability/cldf/values.csv")

# Merge the data
data <- values %>%
  select(Parameter_ID, Language_ID, Value) %>%
  rename(ID = Parameter_ID, variable = Language_ID, value = Value) %>%
  mutate(value = replace(value, which(value == "?"), NA))

data <- as.data.frame(data)

languages <- read_tsv("./admixture/data/languages_map.tsv")

# remove unnecessary features
data <- data[! data$ID %in% FEATURES_TO_IGNORE, ]

data %>% filter(ID =='GB132'& value=='1')
data %>% filter(ID =='GB074'& value=='1')
data %>% filter(ID =='TE066'& value=='0')
data %>% filter(ID =='GB256'& value=='1')
data %>% filter(ID =='GB137'& value=='0')
data %>% filter(ID =='GB028'& value=='1') # exlc/incl

data_family <-  data %>%
  left_join(languages, by = c("variable" = "glottocode"))

data_family %>% 
  filter(family == "Tungusic") %>%
  filter(ID == 'GB059' | ID == 'GB432') %>% # alien/inalien + genitive
  group_by(variable) %>%
  summarize(sum = sum(as.integer(value)))

data_family %>% 
  filter(family == "Tungusic") %>%
  filter(ID == 'GB028' & value == "1")


data_family %>% 
  filter(family == "Mongolic") %>%
  filter(ID == 'GB094' & value == "1")

data_family %>% 
  filter(ID == "GB160") %>%
  filter(family == "Mongolic")

data_family %>% 
  filter(ID == "GB185") %>% # agreement between noun and dem in number
  filter(family == "Mongolic")

data_family %>% 
  filter(ID == "GB184") %>% # agreement between noun and adj in number
  filter(family == "Mongolic")

