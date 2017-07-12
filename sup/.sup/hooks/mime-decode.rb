## turn text/html attachments into plain text, unless they are part
## of a multipart/alternative pair
unless sibling_types.member? "text/plain"
  case content_type
  when "text/html"
    `/usr/bin/w3m -dump '#{filename}'`
  when "multipart/alternative"
    `/usr/bin/w3m -dump '#{filename}'`
  # when "image/jpeg"
  #   `/usr/bin/viewnior '#{filename}'`
  # when "image/png"
  #   `/usr/bin/viewnior '#{filename}'`
  # when "application/octet-stream"
  #   `/usr/bin/viewnior '#{filename}'`
  when "application/msword"
    `/usr/bin/antiword '#{filename}'`
  when "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
    `/usr/bin/docx2txt '#{filename}'`
  # when "application/unknown"
  #   `/usr/bin/run-mailcap --action=view '#{content_type}:#{filename}'`
  # when "application/octet-stream"
  #   `/usr/bin/zathura '#{filename}'`
  end
end
