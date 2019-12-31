function cups
  # luakit/webkit currently doesn't handle authentication dialog properly
  chromium http://localhost:631 &
  # sudo luakit http://localhost:631 &
end
