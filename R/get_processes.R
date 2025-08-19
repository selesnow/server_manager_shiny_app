get_processes <- function() {
  
  analysts_team <- dept::dp_get_team()
  analysts <- names(analysts_team)
  analyst_filter <- str_c(analysts, collapse = '|') %>% str_to_lower()
  
  retry::retry({
  ps::ps() %>% 
    filter(name %in% c('Rterm.exe', 'R.exe', 'python.exe')) %>% 
    rowwise() %>% 
    transmute(
      username = str_split(username, pattern = '\\\\')[[1]][2],
      name,
      status,
      cmdline = list(ps_cmdline(ps_handle)),
      dir     = list(ps_cwd(ps_handle)),
      files   = list(ps_open_files(ps_handle)$path),
      pid     = list(ps_pid(ps_handle)),
      created
    ) %>%
    unnest_longer(cmdline) %>% 
    unnest_longer(dir) %>% 
    unnest_longer(files) %>% 
    unnest_longer(pid) %>% 
    unique() %>% 
    rowwise() %>% 
    mutate(`New Structure` = str_detect(dir, '^C:(\\\\|/)scripts.*')) %>% 
    mutate(
      client = ifelse(
        `New Structure`,
        str_split(dir, pattern = '\\\\|/') %>% pluck(1, 4) %>% to_title_case(),
        'Unknown client'
      )
    ) %>% 
    ungroup() %>% 
    filter(str_detect(tolower(username), analyst_filter)) %>% 
    mutate(update_time = lubridate::with_tz(Sys.time(), "Europe/Kyiv"))
  }, 
  when = 'No such process'
  )
  
}
