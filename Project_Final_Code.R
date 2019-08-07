# -- APAN 5310: SQL & RELATIONAL DATABASES SPRING 2019 ONLINE


# -- START R CODE --

# Import necessary packages
require('RPostgreSQL')

# Load the PostgreSQL driver
drv <- dbDriver('PostgreSQL')

# Create a connection
con <- dbConnect(drv, dbname = 'H2B_visa4',
                 host = 's19db.apan5310.com', port = 50105,
                 user = 'postgres', password = 'y1d8v68m')

# Pass the SQL statements that create all tables
stmt <- "
CREATE TABLE address (
    address_id  varchar(500),
    address      varchar(500),
    city        varchar(30), 
    state       varchar(30), 
    postal_code varchar(30), 
    country     varchar(30),
    county      varchar(30),
    province       varchar(30),
    PRIMARY KEY (address_id)
  );

CREATE TABLE soc_system (
    soc_id    varchar(500),
    soc_code  varchar(30),
    soc_title  varchar(100),
    PRIMARY KEY (soc_id)
  );

CREATE TABLE agents_info (
    agent_id            varchar(500), 
    agent_attorney_name varchar(100),
    PRIMARY KEY (agent_id)
  );

CREATE TABLE employer (
    employer_id   varchar(500),
    employer_name  varchar(100),
    trade_name_dba   varchar(100),
    PRIMARY KEY (employer_id)
  );

CREATE TABLE employer_phone (    
    phone_id  varchar(500),
    employer_id   varchar(500),
    employer_phone  varchar(100),
    PRIMARY KEY (phone_id),
    FOREIGN KEY (employer_id) REFERENCES employer (employer_id)
  );

CREATE TABLE lawfirm (
    lawfirm_id   varchar(500),
    lawfirm_name  varchar(100),
    PRIMARY KEY (lawfirm_id)
  );

CREATE TABLE cases (									
    case_number           varchar(30),
    decision_date         date, 
    visa_type            varchar(30),
    submitted_date        date, 
    case_status           varchar(100), 
    certification_begin_date   date,
    certification_end_date   date,
    agent_poc_employer_rep_by_agent   varchar(30),
    nbr_workers_requested    varchar(500),
    nbr_workers_certified    varchar(500),
    full_time_position    varchar(30),
    nature_of_temporary_need   varchar(100),
    number_of_hours    integer,
    hourly_work_schedule_am    time,
    hourly_work_schedule_pm   time,
    basic_rate_of_pay    numeric(10,2),
    overtime_rate_from   numeric(10,2),
    overtime_rate_to    numeric(10,2),
    pay_range_unit   varchar(30),
    supervise_other_emp   varchar(30),
    supervise_how_many   varchar(30),
    education_level    varchar(30),
    other_education    varchar(100),
    second_diploma    varchar(30),
    training_required    varchar(30),
    emp_experience_reqd    varchar(30),
    emp_exp_num_months    varchar(30),
    other_worksite_location    varchar(30),
    PRIMARY KEY (case_number)
  );

CREATE TABLE job (
    job_id varchar(500),
    job_title varchar(100),
    PRIMARY KEY (job_id)
  );

CREATE TABLE case_lawfirm (
    case_number varchar (30),
    lawfirm_id varchar(500),
    PRIMARY KEY (case_number, lawfirm_id),
    FOREIGN KEY (case_number) REFERENCES cases (case_number),
    FOREIGN KEY (lawfirm_id) REFERENCES lawfirm (lawfirm_id)
  );

CREATE TABLE case_phones (
    case_number varchar (30),
    phone_id varchar(500),
    PRIMARY KEY (case_number, phone_id),
    FOREIGN KEY (case_number) REFERENCES cases (case_number),
    FOREIGN KEY (phone_id) REFERENCES employer_phone (phone_id)
  );

CREATE TABLE case_agents (
    case_number varchar (30),
    agent_id    varchar(500),
    PRIMARY KEY (case_number, agent_id),
    FOREIGN KEY (case_number) REFERENCES cases (case_number),
    FOREIGN KEY (agent_id) REFERENCES agents_info (agent_id)
  );

CREATE TABLE case_job (
    case_number varchar (30),
    job_id   varchar(500),
    PRIMARY KEY (case_number, job_id),
    FOREIGN KEY (case_number) REFERENCES cases (case_number),
    FOREIGN KEY (job_id) REFERENCES job (job_id)
  );

CREATE TABLE address_employers (
    employer_id    varchar (500),
    address_id   varchar (500),
    case_number  varchar(30),
    PRIMARY KEY (employer_id, address_id, case_number),
    FOREIGN KEY (employer_id) REFERENCES employer (employer_id),
    FOREIGN KEY (address_id) REFERENCES address (address_id),
    FOREIGN KEY (case_number) REFERENCES cases (case_number)
  );

CREATE TABLE address_agents (
    agent_id    varchar (500),
    address_id   varchar (500),
    case_number  varchar (30),
    PRIMARY KEY (agent_id, address_id, case_number),
    FOREIGN KEY (agent_id) REFERENCES agents_info (agent_id),
    FOREIGN KEY (address_id) REFERENCES address (address_id),
    FOREIGN KEY (case_number) REFERENCES cases (case_number)
  );

CREATE TABLE address_employee (
    case_number varchar(30),
    address_id    varchar (500),
    PRIMARY KEY (case_number, address_id),
    FOREIGN KEY (case_number) REFERENCES cases (case_number),
    FOREIGN KEY (address_id) REFERENCES address (address_id)
  );

CREATE TABLE job_soc (
    job_id    varchar (500),
    soc_id   varchar (500),
    PRIMARY KEY (job_id, soc_id),
    FOREIGN KEY (job_id) REFERENCES job (job_id),
    FOREIGN KEY (soc_id) REFERENCES soc_system (soc_id)
  );

CREATE TABLE naics (
    naics_id   varchar (500),
    naics_code   varchar (500),
    employer_id   varchar (500),
    PRIMARY KEY (naics_id),
    FOREIGN KEY (employer_id) REFERENCES employer (employer_id)
  );

CREATE TABLE case_naics (
    case_number varchar(30),
    naics_id    varchar (500),
    PRIMARY KEY (case_number, naics_id),
    FOREIGN KEY (case_number) REFERENCES cases (case_number),
    FOREIGN KEY (naics_id) REFERENCES naics (naics_id)
  );

CREATE TABLE major (
    major_id   varchar (500),
    major   varchar(300),
    PRIMARY KEY (major_id)
  );

CREATE TABLE case_major (
    case_number varchar(30),
    major_id    varchar (500),
    PRIMARY KEY (case_number, major_id),
    FOREIGN KEY (case_number) REFERENCES cases (case_number),
    FOREIGN KEY (major_id) REFERENCES major (major_id)
  );
"

# Execute the statement to create tables
dbGetQuery(con, stmt)

# Load the csv file in a dataframe, df
df <- read.csv('H-2B_Disclosure_Data_FY2018_EOY.csv', stringsAsFactors=FALSE)

# Make dataframe columns lowercase to match PostgreSQL
names(df) <- tolower(names(df))
head(df)

# basic_rate_of_pay have $ sign, convert to numeric
str(df$basic_rate_of_pay)
df$basic_rate_of_pay = as.numeric(gsub("\\$", "", df$basic_rate_of_pay))
head(df$basic_rate_of_pay)

# Replace empty dates and time with NA
df$decision_date[df$decision_date == ''] <- NA
df$submitted_date[df$submitted_date == ''] <- NA
df$certification_begin_date[df$certification_begin_date == ''] <- NA
df$certification_end_date[df$certification_end_date == ''] <- NA
df$hourly_work_schedule_am[df$hourly_work_schedule_am == ''] <- NA
df$hourly_work_schedule_pm[df$hourly_work_schedule_pm == ''] <- NA

# Replace other NA columns with empty space
df$employer_phone[is.na(df$employer_phone)] <- ''
df$trade_name_dba[is.na(df$trade_name_dba )] <- ''
df$major[is.na(df$major)] <- ''


###### Create the "address" dataframe of unique values and add the address_id
df$employer_address <- paste(df$employer_address_1, df$employer_address_2)
df$employer_county=''
empl_colnames <- c('employer_address', 'employer_city', 'employer_state', 
                   'employer_postal_code', 'employer_country', 'employer_province','employer_county')
df$employer_province[is.na(df$employer_province)]=''
#df$employer_province[df$employer_province=='N/A']=''
empl_address_df <- df[empl_colnames][!duplicated(df[empl_colnames]),]
colnames(empl_address_df) <- c('address', 'city', 'state', 'postal_code', 'country', 'province','county')

df$agent_attorney_city[is.na(df$agent_attorney_city)]=''
df$agnt_address <- ''
df$agnt_postal_code <- ''
df$agnt_country <- ''
df$agnt_province <- ''
df$agnt_county <- ''
agnt_address_df <- df[c('agnt_address', 'agent_attorney_city', 'agent_attorney_state', 'agnt_postal_code', 'agnt_country', 'agnt_province','agnt_county')][!duplicated(df[c('agnt_address', 'agent_attorney_city', 'agent_attorney_state', 'agnt_postal_code', 'agnt_country', 'agnt_province','agnt_county')]),]
colnames(agnt_address_df) <- c('address', 'city', 'state', 'postal_code', 'country', 'province','county')
address_df <- rbind(empl_address_df, agnt_address_df)

df$employee_country <- ''
df$employee_province <- ''
df$employee_address<-''
employee_address_df <-  df[c('employee_address','employee_worksite_city','employee_work_state','employee_postal_code','employee_country','employee_province','employee_worksite_county')][!duplicated(df[c('employee_address','employee_worksite_city','employee_work_state','employee_postal_code','employee_country','employee_province','employee_worksite_county')]),]
colnames(employee_address_df) <- c('address', 'city', 'state', 'postal_code', 'country', 'province','county')
address_df <- rbind(address_df, employee_address_df)
address_df=address_df[c('address', 'city', 'state', 'postal_code', 'country', 'province','county')][!duplicated(address_df[c('address', 'city', 'state', 'postal_code', 'country', 'province','county')]),]
address_df$address_id=1:nrow(address_df)

# Push the "address" dataframe to database
dbWriteTable(con, name='address', value=address_df, row.names=FALSE, append=TRUE)

###### Create the "soc_system" dataframe of unique soc codes and names and add the soc_id
socs_df <- df[c('soc_code', 'soc_title')][!duplicated(df[c('soc_code', 'soc_title')]),]
socs_df$soc_id <- 1:nrow(socs_df)

# Push the "soc_system" dataframe to database
dbWriteTable(con, name='soc_system', value=socs_df, row.names=FALSE, append=TRUE)

###### Create the "agents_info" dataframe of unique agent names and add the agent_id
agents_df <- data.frame('agent_attorney_name' = unique(df$agent_attorney_name))
agents_df$agent_id <- 1:nrow(agents_df)
agents_df <- agents_df[complete.cases(agents_df), ]

# Push the "agents_info" dataframe to database
dbWriteTable(con, name='agents_info', value=agents_df, row.names=FALSE, append=TRUE)

###### Create the "employer" dataframe of unique employer names and and trade_name_dba add the employer_id
employer_df <- df[c('employer_name','trade_name_dba')][!duplicated(df[c('employer_name', 'trade_name_dba')]),]
employer_df$employer_id <- 1:nrow(employer_df)

# Push the "employer" dataframe to database
dbWriteTable(con, name='employer', value=employer_df, row.names=FALSE, append=TRUE)

###### Add the "employer_id" to the main dataframe, df, using employer_df as a lookup table
df=merge(df,employer_df,by.x = c('employer_name', 'trade_name_dba'),by.y = c('employer_name', 'trade_name_dba'),no.dups = TRUE,sort=FALSE,all.x=T)

employer_id_list <- apply(df[c('employer_name', 'trade_name_dba')], 1, function(x) { employer_df$employer_id[(employer_df$employer_name == x[1]) & (employer_df$trade_name_dba == x[2])] })
df$employer_id <- employer_id_list

# Create the "employer_phone" dataframe of unique employer_phone and add the phone_id and employer_id
phone_df <- df[c('employer_phone','employer_id')][!duplicated(df[c('employer_phone', 'employer_id')]),]
phone_df$phone_id <- 1:nrow(phone_df)

# Push the "employer_phone" dataframe to database
dbWriteTable(con, name='employer_phone', value=phone_df, row.names=FALSE, append=TRUE)

###### Create the "lawfirm" dataframe of unique lawfirm names and add the lawfirm_id
lawfirm_df <- data.frame('lawfirm_name' = unique(df$lawfirm_name))
lawfirm_df$lawfirm_id <- 1:nrow(lawfirm_df)
lawfirm_df <- lawfirm_df[complete.cases(lawfirm_df), ]  

# Push the "lawfirm" dataframe to database
dbWriteTable(con, name='lawfirm', value=lawfirm_df, row.names=FALSE, append=TRUE)

###### Create the "cases" dataframe 
cases_df <- df[c('case_number', 'decision_date', 'visa_type', 'submitted_date', 
                 'case_status', 'certification_begin_date', 'certification_end_date', 
                 'agent_poc_employer_rep_by_agent', 'nbr_workers_requested',
                'nbr_workers_certified', 'full_time_position', 'nature_of_temporary_need',
                'number_of_hours', 'hourly_work_schedule_am', 'hourly_work_schedule_pm', 
                'basic_rate_of_pay', 'overtime_rate_from', 'overtime_rate_to', 'pay_range_unit',
                'supervise_other_emp', 'supervise_how_many', 'education_level', 'other_education',
                'second_diploma', 'training_required', 'emp_experience_reqd', 'emp_exp_num_months',
                'other_worksite_location')]

# Push the "cases" dataframe to database
dbWriteTable(con, name='cases', value=cases_df, row.names=FALSE, append=TRUE)

###### Create the "job" dataframe of unique job title and add the job_id
job_df <- data.frame('job_title' = unique(df$job_title))
job_df$job_id <- 1:nrow(job_df)
job_df <- job_df[complete.cases(job_df), ]

# Push the "job" dataframe to database
dbWriteTable(con, name='job', value=job_df, row.names=FALSE, append=TRUE)

###### Add the "lawfirm_id" to the main dataframe, df, using lawfirm_df as a lookup table
lawfirm_id_list <- sapply(df$lawfirm_name, function(x) lawfirm_df$lawfirm_id[lawfirm_df$lawfirm_name == x])
df$lawfirm_id <- lawfirm_id_list

# Slice the main dataframe to get the "case_lawfirm" table
case_lawfirm_df <- df[c('case_number', 'lawfirm_id')][!duplicated(df[c('case_number', 'lawfirm_id')]),]

# Push the "cases_agents" dataframe to database
dbWriteTable(con, name='case_lawfirm', value=case_lawfirm_df, row.names=FALSE, append=TRUE)

###### Add the "phone_id" to the main dataframe, df, using socs_df as a lookup table
df=merge(df,phone_df,by.x = c('employer_phone', 'employer_id'),by.y = c('employer_phone', 'employer_id'),no.dups = TRUE,sort=FALSE,all.x=T)
                          
# Slice the main dataframe to get the "case_phones" table
case_phones_df <- df[c('case_number', 'phone_id')][!duplicated(df[c('case_number', 'phone_id')]),]

# Push the "case_phones" dataframe to database
dbWriteTable(con, name='case_phones', value=case_phones_df, row.names=FALSE, append=TRUE)
                        
###### Add the "agent_id" to the main dataframe, df, using agents_df as a lookup table
agent_id_list <- sapply(df$agent_attorney_name, function(x) agents_df$agent_id[agents_df$agent_attorney_name == x])
df$agent_id <- agent_id_list

# Slice the main dataframe to get the "case_agents" table
case_agents_df <- df[c('case_number', 'agent_id')][!duplicated(df[c('case_number', 'agent_id')]),]

# Push the "cases_agents" dataframe to database
dbWriteTable(con, name='case_agents', value=case_agents_df, row.names=FALSE, append=TRUE)

###### Add the "job_id" to the main dataframe, df, using job_df as a lookup table
job_id_list <- sapply(df$job_title, function(x) job_df$job_id[job_df$job_title == x])
df$job_id <- job_id_list

# Slice the main dataframe to get the "case_job" table
case_job_df <- df[c('case_number', 'job_id')][!duplicated(df[c('case_number', 'job_id')]),]

# Push the "case_job" dataframe to database
dbWriteTable(con, name='case_job', value=case_job_df, row.names=FALSE, append=TRUE)

###### Add the "address_df" to the main dataframe
df=merge(df,address_df,by.x = c('employer_address', 'employer_city', 'employer_state', 'employer_postal_code', 'employer_country', 'employer_province','employer_county'),by.y = c('address','city','state','postal_code','country','province','county'),no.dups = TRUE,sort=FALSE,all.x=T)
colnames(df)[76] <- "employer_address_id"
                      
# Add the "address_employer_df" to the main dataframe
address_employer_df <-df[c('employer_id', 'employer_address_id','case_number')][!duplicated(df[c('employer_id', 'employer_address_id','case_number')]),]
colnames(address_employer_df) <- c('employer_id','address_id','case_number')

# Push the "address_employer" dataframe to database
dbWriteTable(con, name='address_employers', value=address_employer_df, row.names=FALSE, append=TRUE)

###### Add the "address_df" to the main dataframe
df=merge(df,address_df,by.x = c('agnt_address', 'agent_attorney_city', 'agent_attorney_state', 'agnt_postal_code', 'agnt_country', 'agnt_province','agnt_county'),by.y = c('address','city','state','postal_code','country','province','county'),no.dups = TRUE,sort=FALSE,all.x=T)
colnames(df)[77] <- "address_id"
                      
# Add the "address_agents_df" to the main dataframe
address_agents_df <- df[c('agent_id', 'address_id','case_number')][!duplicated(df[c('agent_id', 'address_id','case_number')]),]
colnames(address_agents_df) <- c('agent_id','address_id','case_number')

# dbWriteTable(con, name='address_agents', value=address_agents_df, row.names=FALSE, append=TRUE)
dbWriteTable(con, name='address_agents', value=address_agents_df, row.names=FALSE, append=TRUE)
                      
###### Add the "address_df" to the main dataframe
df=merge(df,address_df,by.x = c('employee_address','employee_worksite_city','employee_work_state','employee_postal_code','employee_country','employee_province','employee_worksite_county'),by.y = c('address','city','state','postal_code','country','province','county'),no.dups = TRUE,sort=FALSE)
colnames(df)[78] <- "address_id"

# Add the "address_employee_df" to the main dataframe
address_employee_df <-df[c('case_number', 'address_id')][!duplicated(df[c('case_number', 'address_id')]),]
colnames(address_employee_df) <- c('case_number','address_id')
                      
# dbWriteTable(con, name='address_employee', value=address_agents_df, row.names=FALSE, append=TRUE)              
dbWriteTable(con, name='address_employee', value=address_employee_df, row.names=FALSE, append=TRUE)

###### Add the "soc_id" to the main dataframe, df, using socs_df as a lookup table
soc_id_list <- apply(df[c('soc_code', 'soc_title')], 1, function(x) { socs_df$soc_id[(socs_df$soc_code == x[1]) & (socs_df$soc_title == x[2])] })
df$soc_id <- soc_id_list

#Slice the main dataframe to get the "job_soc" table
job_soc_df <- df[c('job_id', 'soc_id')][!duplicated(df[c('job_id', 'soc_id')]),]

# Push the "case_job" dataframe to database
dbWriteTable(con, name='job_soc', value=job_soc_df, row.names=FALSE, append=TRUE)
                
###### create 'naics' dataframe of unique code and add the naics_id
naics_df <- df[c('naics_code', 'employer_id')][!duplicated(df[c('naics_code', 'employer_id')]),]
naics_df$naics_id <- 1:nrow(naics_df)

# Push the "naics" dataframe to database
dbWriteTable(con, name='naics', value=naics_df, row.names=FALSE, append=TRUE)

######## Add the "naics_id" to the main dataframe
df<- merge(df,naics_df,by.x = c('naics_code','employer_id'),by.y = c('naics_code','employer_id'),no.dups = TRUE,sort=FALSE)

#Slice the main dataframe to get the "case_naics" table
case_naics_df <- df[c('case_number','naics_id')][!duplicated(df[c('case_number','naics_id')]),]

#Push the "case_naics" dataframe to database
dbWriteTable(con, name='case_naics', value=case_naics_df, row.names=FALSE, append=TRUE)

###### Create the "major" dataframe of unique major and add the major_id
major_df <- data.frame('major' = unique(df$major))
major_df$major_id <- 1:nrow(major_df)
major_df <- major_df[complete.cases(major_df), ]

# Push the "agents_info" dataframe to database
dbWriteTable(con, name='major', value=major_df, row.names=FALSE, append=TRUE)

###### Add the "major_id" to the main dataframe, df, using agents_df as a lookup table
major_id_list <- sapply(df$major, function(x) major_df$major_id[major_df$major == x])
df$major_id <- major_id_list

# Slice the main dataframe to get the "case_major" table
case_major_df <- df[c('case_number', 'major_id')][!duplicated(df[c('case_number', 'major_id')]),]

# Push the "case_major" dataframe to database
dbWriteTable(con, name='case_major', value=case_major_df, row.names=FALSE, append=TRUE)

# -- END R CODE --

# -----------------------------------------------------------------------------

