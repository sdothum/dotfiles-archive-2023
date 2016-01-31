function slrn
  switch "$argv[1]"
    case [1];   command slrn -h news.sunnyusenet.com
    case I;     command slrn -h news.sunnyusenet.com --create
                  and command slrn -h news.sunnyusenet.com -d
    case '*';   echo ".. slrn  'start  I'nitialize"
  end
end
