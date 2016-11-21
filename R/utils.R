buildQueryBody = function(...) {
  docs = list(...)
  docs$sep = ","
  sprintf('{"documents": [%s]}',do.call(paste, docs))
}
