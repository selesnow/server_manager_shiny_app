
delete_desktop_ini <- function(path = ".") {
  ps_command <- glue::glue(
    "Get-ChildItem -Path '{normalizePath(path)}' -Filter 'desktop.ini' -Recurse -Force -ErrorAction SilentlyContinue | Remove-Item -Force -ErrorAction SilentlyContinue"
  )
  
  system2("powershell", args = c("-Command", ps_command))
}

delete_desktop_ini()
