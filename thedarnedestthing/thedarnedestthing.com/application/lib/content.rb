# encoding: UTF-8

# the darnedest thing
# ▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬

module Content
  class << self

    # ....................................................... Home / thread page

    def homepage(title)
      LOG.info(%[Building "thread" page => #{title}])
      intro = Intro.new(title)
      content = intro.content
      diary = Diary.new(title)
      content << diary.content
      history = Footer.new('history')
      # insert most recent thread content
      unless title == HOMEPAGE
        wikis = WikiList.new(intro.thread)
        content << '<div class="content">'
        date = nil
        wikis.directory.each do |filename|
          uri = File.uri(filename)
          wiki = Wiki.new(uri)
          if date.nil? or wiki.publishing[:date] == date
            unless wiki.empty?
              content << wiki.body
              date ||= wiki.publishing[:date]
            end
          else
            history << wiki.href unless HideArticle.call(wiki.homepage, (wiki.draft or wiki.lock))
          end
        end
      end
      content << yield if block_given?
      # add diary journal links to journal footer
      if title == HOMEPAGE
        wikis = WikiList.new(diary.thread)
        wikis.directory.each do |filename|
          uri = File.uri(filename)
          wiki = Wiki.new(uri)
          history << EscapeHtml.call(wiki.href) unless HideArticle.call(wiki.homepage, (wiki.draft or wiki.lock))
        end
      end
      content << history.links + diary.history.links
      content << '</div>'
    end

    # ........................................................ Recent posts list
    
    def recent_posts
      LOG.info('Assembling recent posts')
      list = []
      THREADS.each do |thread, homepage|
        # unless homepage == HOMEPAGE or HideArticle.call(thread)
        unless HideArticle.call(thread)
          wikis = WikiList.new(thread)
          date = nil
          wikis.directory.each do |filename|
            # ignore journal entries
            unless IsDate.call(filename)
              title = File.title(filename)
              if date.nil? or File.date(filename) == date
                wiki = Wiki.new(title)
                unless wiki.empty?
                  list << [File.stat(filename).mtime, wiki.profile]
                  date ||= wiki.publishing[:date] unless wiki.draft
                end
              else
                break unless not IsVPS.call
                wiki = Wiki.new(title)
                list << [File.stat(filename).mtime, wiki.profile] if wiki.draft
              end
            end
          end
        end
      end
      # insert most recent article for each thread
      content = '<div class="content">'
      threads = []
      omit=File.exist?(OMIT) ? IO.read(OMIT) : ''
      date = nil
      list.sort.reverse.each do |post|
        # only list one article per thread and excluding home page
        unless threads.include? post[1][4] or post[1][4] == HOMEPAGE
          if omit.scan(/#{post[1][0]}/) == []
            date ||= Date.parse("#{post[0]}")
            # only list article within cutoff period
            if (date - Date.parse("#{post[0]}")).to_i < OMIT_CUTOFF
              wiki = Wiki.new(post[1][0])
              content << wiki.body
              threads << post[1][4]
            end
          end
        end
      end
      content << '</div>'
      # list mini index of recent posts
      content << '<div class="index"><h1 class="index"><a href="/recent">recent posts</a></h1>'
      list.sort.reverse.each { |post| content << index_item(post[1]) }
      content << '</div>'
      content
    end

    # ..................................................... Recent comments page
    
    def recent_comments(month)
      LOG.info('Building "recent comments" page')
      emails = (Dir.glob("#{WIKI}/mail/comments/**/*").sort_by { |filename| File.mtime(filename) }).reverse
      # LOG.info("emails :: #{emails}")
      content = '<div id="results">'
      history = Footer.new('history')
      date = nil
      emails.each do |filename|
        next if File.directory?(filename)
        publishing = DayMonthYear.call(File.date(filename))
        if publishing[:month] == month or month.nil?
          unless publishing[:date] == date
            date = publishing[:date]
            content << "<h2>#{date}</h2>"
            # content << %[<h2><a href="/date?period=#{publishing[:year]}##{publishing[:month]}">#{date}</a></h2>]
          end
          month = publishing[:month]
          comment = Comment.new(filename)
          content << snippet(comment)
        else
          LOG.debug("Comment => #{filename.sub(/.*\/comments\//, '')}")
          history << %[<a href="/recent comments?period=#{publishing[:month]}">#{publishing[:month]}</a>]
        end
      end
      content << history.links
      content << '</div>'
    end

    # ................................................................ Blog page

    def blog(thread, month)
      LOG.info(%[Building "#{thread} #{month} blog" page])
      wikis = BlogList.new(thread)
      content = '<div id="results">'
      history = Footer.new('history')
      date = nil
      wikis.directory.each do |filename|
        uri = File.uri(filename)
        wiki = Wiki.new(uri)
        unless wiki.empty?
          if wiki.publishing[:month] == month or month.nil?
            unless wiki.publishing[:date] == date
              date = wiki.publishing[:date]
              content << %[<h2><a href="/date?period=#{wiki.publishing[:year]}##{wiki.publishing[:month]}">#{date}</a></h2>]
            end
            month = wiki.publishing[:month]
            content << snippet(wiki, :show_comment)
          else
            history << %[<a href="/#{thread}?period=#{wiki.publishing[:month]}">#{wiki.publishing[:month]}</a>]
          end
        end
      end
      content << history.links
      content << "</div>"
    end

    # ..................................................... Recent articles page

    def recent(month)
      LOG.info(%[Building "recent #{month}" page])
      wikis = WikiList.new
      content = '<div id="results">'
      history = Footer.new('history')
      date = nil
      wikis.directory.each do |filename|
        uri = File.uri(filename)
        wiki = Wiki.new(uri)
        unless wiki.empty?
          if wiki.publishing[:month] == month or month.nil?
            unless wiki.publishing[:date] == date
              date = wiki.publishing[:date]
              content << %[<h2><a href="/date?period=#{wiki.publishing[:year]}##{wiki.publishing[:month]}">#{date}</a></h2>]
            end
            month = wiki.publishing[:month]
            content << snippet(wiki, :show_comment)
          else
            history << %[<a href="/recent?period=#{wiki.publishing[:month]}">#{wiki.publishing[:month]}</a>]
          end
        end
      end
      content << history.links
      content << "</div>"
    end

    # .................................................... Articles by date page

    def date(year)
      LOG.info(%[Building "date #{year}" page])
      wikis = WikiList.new
      content = '<div id="results">'
      history = Footer.new('history')
      month = nil
      wikis.directory.each do |filename|
        uri = File.uri(filename)
        wiki = Wiki.new(uri)
        unless wiki.empty?
          if wiki.publishing[:year] == year or year.nil?
            unless wiki.publishing[:month] == month
              month = wiki.publishing[:month]
              content << %[<h2><a name="#{month}">#{month}</a></h2>]
            end
            year = wiki.publishing[:year]
            content << index_item(wiki.profile)
          else
            history << %[<a href="/date?period=#{wiki.publishing[:year]}">#{wiki.publishing[:year]}</a>]
          end
        end
      end
      content << history.links
      content << "</div>"
    end

    # ........................................................Gallery index page

    def gallery(*)
      LOG.info('Building "gallery" page')
      galleries = Dir.glob("#{IMAGES}/*/**/")
      content = '<div id="results">'
      section, group = nil, nil
      galleries.sort.each do |gallery|
        if Dir.glob("#{gallery}*.jpg").count > 0
          title = gallery.split('/')[-1]
          LOG.info("#{title} :: #{gallery.split('/')}")
          unless GalleryName.call(gallery.split('/')[-3]) == 'gallery'
            unless gallery.split('/')[-3] == section
              section = gallery.split('/')[-3]
              content << %[</ul><h1 class="gallery">#{GalleryName.call(section.downcase)}</h1>]
              group = nil
            end
          end
          # root contains wiki (non-gallery) images/screenshots only
          unless title == 'images'
            unless gallery.split('/')[-2] == group
              group = gallery.split('/')[-2]
              content << %[</ul><h2>#{GalleryName.call(group.downcase)}</h2><ul class="rig columns-3">]
            end
            list = Dir.glob("#{IMAGES}/**/#{title}/*_1.jpg")
            list = Dir.glob("#{IMAGES}/**/#{title}/*.jpg") unless list
            # count = list.count
            thumbnail = list[0]
            thumbnail = ImageUri.call(thumbnail)
            content << %[<li><a href="/gallery/#{title}"><img src="#{thumbnail}"><p>#{GalleryName.call(title.downcase)}</p></a></li>]
          end
        end
      end
      content << '</ul></div>'
    end

    # ............................................................... Index page

    def index(*)
      LOG.info('Building "index" page')
      wikis = WikiList.new(:index)
      content = '<div id="results">'
      letter = nil
      (wikis.index.sort { |a,b| a.downcase <=> b.downcase }).each do |title|
        thread_name = wikis.thread_name(title)
        next if title == 'index'
        uri = File.uri("#{WIKI}/#{thread_name}/#{title}.#{VIMWIKI_EXT}")
        wiki = Wiki.new(uri)
        unless wiki.empty?
          unless title[0].downcase == letter
            letter = title[0].downcase
            content << "<h2>#{letter}</h2>"
          end
          content << index_item(wiki.profile)
        end
      end
      content << '</div>'
    end

    def index_item(profile)
      %[<span class="block"><a class="left" href="/#{profile[0]}">#{TOC.call(profile[1..4])}<span class="right">#{profile[4]}</span></a></span>]
    end

    def snippet(item, show_comment = nil)
      content = %[<p><span class="article">#{item.href}</span>&nbsp;&ndash;]
      content << %[<span class="opening">#{item.plain_text(PREVIEW_WORDS)} ... </span>]
      content << %[<span class="thread">#{item.homepage}#{"&nbsp;&nbsp;&nbsp;#{item.comments_href}" if show_comment and item.comments?}</span></p>]
    end
  end
end

