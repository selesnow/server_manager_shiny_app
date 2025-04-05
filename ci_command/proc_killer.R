library(ps) 
library(purrr)
library(dplyr)

# file to fill proc
stop_script <- "C:\\scripts\\alsey\\netpeak_core\\nc_analytics_team\\telegram_bot\\netpeak_tg_bot.R"

# load all r proccess
print('read procs')
all_r_processes <- ps::ps(user = "WIN-BTJ7HOEDRIG\\Alsey") %>% 
                   filter(name %in% c('Rterm.exe', 'R.exe'))

# collect process details
print('get procs details')
proc <- purrr::map(all_r_processes$ps_handle, 
    possibly({
     ~ 
      list(
        cmdline = ps_cmdline(.x),
        dir     = ps_cwd(.x),
        files   = ps_open_files(.x),
        pid     = ps_pid(.x),
        proc    = .x
      )
    }
    )
)

print('start proc killer cicle')
for (p in proc) {
  
  if (is.null(p)) next
  
  if (stop_script %in% p$files$path) {
    print(ps_kill(p$proc))
  }
  
}

