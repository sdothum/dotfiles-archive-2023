function xp
  xprop | grep "WM_WINDOW_ROLE\\|WM_CLASS"; and echo "WM_CLASS(STRING) = \"NAME\", \"CLASS\""
end
