# encoding: UTF-8

# the darnedest thing
# ▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬

module Admin
  class << self

    # .......................................................... Admin functions

    def each_wiki(description)
      THREADS.each do |thread, homepage|
        LOG.info(%[#{description} "#{thread}"])
        Dir.glob("#{WIKI}/#{thread}/*.#{VIMWIKI_EXT}").each { |filename| yield filename }
      end
    end

    def each_diary(description)
      THREADS.each do |thread, homepage|
        LOG.info(%[#{description} "#{thread}/diary"])
        Dir.glob("#{WIKI}/#{thread}/diary/*.#{VIMWIKI_EXT}").each { |filename| yield filename }
      end
    end

    def wiki_references(filename)
      IO.read(filename)
        .scan(/\[\[[^\[]*\]\]/)
        .delete_if { |link| link =~ /mailto:/ }
        .map { |link| link.gsub(/\n/m, '') }
        .map { |link| link.gsub(/#[^\|]*/, '')}
        .map { |link| link.gsub(/\[\[(https*:\/\/[^\]]*)[|]([^\[]*)\]\]/, '') }
        .map { |link| link.gsub(/[^\]]*\[\[([^\]]*)[|]([^\[]*)\]\][^\[]*/, '[[\1]]') }
        .uniq
        .map { |link| link.sub(/\[\[(.*)\]\]/, '\1') }
    end

   # ......................................................... Sync diary dates

    def sync_diary_dates
      each_diary('Restoring timestamps for') { |filename| File.date_filename(filename) }
    end

    # .................................................... Cross reference wikis

    def xref_wikis
      FileUtils.rm_rf("#{XREF}")
      each_wiki('Creating wiki xref for') { |filename| Admin.wiki_xref(filename) }
    end

    # ..................................................... Save wiki timestamps

    def save_timestamps
      each_wiki('Saving timestamps for') { |filename| File.save_timestamp(filename) }
    end

    # .................................................. Restore wiki timestamps

    def restore_timestamps
      each_wiki('Restoring timestamps for') { |filename| File.restore_timestamp(filename) }
    end

    # ..................................................... Vacuum wiki database

    def vacuum_wikis
      each_wiki('Cleaning empty articles for') { |filename| File.delete(filename) if File.zero?(filename) }
    end

    # .............................................................. List drafts

    def wiki_drafts
      content = '<div id="results">'
      THREADS.each do |thread, homepage|
        LOG.info(%[Checking for draft "#{homepage}" articles])
        content << "<h2>#{homepage}</h2>"
        Dir.glob("#{WIKI}/#{thread}/**/*.#{VIMWIKI_EXT}").each do |filename|
          unless File.zero?(filename)
            title = File.title(filename)
            LOG.debug(%[title => "#{title}"])
            if Wiki.new(title).draft
              LOG.warning("#{homepage} /#{title}/ draft => #{title.red}")
              content << %[<span class="block"><a href="/#{Blog.call(thread)}#{title}">#{title}<span class="right">#{thread.sub(/\/diary$/, '')}</span></a></span>]
            end
          end
        end
      end
      content << '</div>'
    end

    # ................................................... List orphan references

    def wiki_orphans
      content = '<div id="results">'
      THREADS.each do |thread, homepage|
        LOG.info(%[Checking for orphan "#{homepage}" articles])
        content << "<h2>#{homepage}</h2>"
        Dir.glob("#{WIKI}/#{thread}/**/*.#{VIMWIKI_EXT}").each do |filename|
          unless File.zero?(filename)
            wiki_references(filename).each do |title|
              LOG.debug(%[title => "#{title}"])
              if Wiki.new(title).empty?
                wiki = File.title(filename)
                LOG.warning("#{homepage} /#{wiki}/ orphan => #{title.red}")
                content << %[<span class="block"><a href="/#{wiki}">#{title}<span class="right">#{wiki}</span></a></span>]
              end
            end
          end
        end
      end
      content << '</div>'
    end

    # ..................................................... Build xref directory

    def wiki_xref(filename)
      unless File.title(filename) =~ /index|diary/
        wiki_references(filename).each do |title|
          LOG.debug(%[#{File.title(filename)} => "#{title}"])
          FileUtils.mkdir_p("#{WIKI}/xref/#{title}") unless Dir.exist?("#{WIKI}/xref/#{title}")
          system(%[touch "#{WIKI}/xref/#{title}/#{File.title(filename)}"])
        end
      end
    end

    # .......................................................... Update comments

    def parse_mail(message)
      date = message.date.strftime("%Y-%m-%d").split('-')
      if message.multipart?
        text = ''
        message.parts.each do |part|
          text = text + (
            case part.mime_type
              when 'text/plain' then "#{part.body.decoded}".gsub(/\r\n/m, "\n")
              when 'text/html' then "#{part.body.decoded}".gsub(/<.*>/m, '').gsub(/.*>/m, '').gsub(/<.*/m, '').squeeze(' ').gsub(/\n /m, "\n")
              when /multipart\/.*/ then "#{message.text_part.body.decoded}".gsub(/\r\n/m, "\n")
              else ''
            end
          ).gsub(/^ +/m, '')
          break if text.length > 1
        end
      else
        text = (
          case message.mime_type
            when 'text/plain' then "#{message.body}"
            when 'text/html' then "#{message.body.decoded}".gsub(/<.*>/m, '').gsub(/.*>/m, '').gsub(/<.*/m, '').squeeze(' ').gsub(/\n /m, "\n")
            when /multipart\/.*/ then "#{message.text_part.body.decoded}".gsub(/\r\n/m, "\n")
            else ''
          end
        ).gsub(/^ +/m, '')
      end
      text.gsub(/\n\n/, "\n")
    end

    def wiki_mail
      LOG.info('Processing "email" comments')
      wikis = WikiList.new
      galleries = GalleryList.new
      emails = Dir.glob("#{MAIL}/{cur,new}/*")
      spammers = %[~#{IO.read("#{WIKI}/mail/rules/spammers").split("\n").join('~').downcase}~]
      titles = "~#{wikis.index.join('~').downcase}~#{(galleries.index.map { |gallery| "gallery/#{gallery}" }).join('~')}~"
      LOG.debug("spammers => #{spammers}")
      LOG.debug("titles => #{titles}")
      emails.each do |mail|
        post = File.basename(mail)
        LOG.debug("post => #{post}")
        if Dir.glob("#{WIKI}/mail/{comments,junk,pending,rejected,empty}/**/#{post}").empty?
          message = Mail.read(mail)
          LOG.debug("message => #{message}")
          if spammers.rindex("~#{message.from[0].downcase}~")
            LOG.error("spam => #{post} subject => #{message.subject} spammers => #{message.from[0]}")
            FileUtils.touch("#{WIKI}/mail/junk/#{post}")
            next
          end
          subject = message.subject.gsub(/%20/, ' ').sub(/.*\/\/\/( *(gallery\/)*([^\/]*))\/\/\/.*/, '\1').downcase.strip.squeeze(' ')
          LOG.debug("subject => #{subject}")
          unless titles.rindex("~#{subject}~")
            LOG.error("invalid subject => #{post} subject => #{message.subject} from => #{message.from[0]}")
            FileUtils.touch("#{WIKI}/mail/rejected/#{post}")
            next
          end
          if message.body.empty?
            LOG.warning("empty post => #{post} subject => #{subject}")
            FileUtils.mkdir_p("#{WIKI}/mail/empty/#{subject}") unless Dir.exist?("#{WIKI}/mail/empty/#{subject}")
            FileUtils.touch("#{WIKI}/mail/empty/#{subject}/#{post}")
            next
          end
          begin
            LOG.info("comments post => #{post} subject => #{subject}")
            text = Admin.parse_mail(message)
            raise if text.length == 0
            raise if text.split("\n")[0] =~ /^-- *$/
            FileUtils.mkdir_p("#{WIKI}/mail/comments/#{subject}") unless Dir.exist?("#{WIKI}/mail/comments/#{subject}")
            comment = File.new("#{WIKI}/mail/comments/#{subject}/#{post}", 'w')
            comment.write(%[<p class="date">#{FormatDate.call(Time.gm(date[0], date[1], date[2]))}</p>\n])
            from = %[<span class="sender">#{message.from[0].sub(/(.*)@.*/, '\1').gsub(/\./, ' ')}</span> said: ]
            text.split("\n").each do |line|
              break if line =~ /^-- *$/
              comment.write(%[<p class="comment">#{from}#{HideEmail.call(HidePhone.call(line))}</p>\n])
              from = ''
            end
            comment.close
            FileUtils.touch("#{WIKI}/mail/comments/#{subject}/#{post}", :mtime => message.date.to_time)
          rescue
            LOG.warning("pending post: #{post} -> subject: #{subject} -> mime_type: #{message.mime_type}}")
            FileUtils.mkdir_p("#{WIKI}/mail/pending/#{subject}") unless Dir.exist?("#{WIKI}/mail/pending/#{subject}")
            FileUtils.touch("#{WIKI}/mail/pending/#{subject}/#{post}")
          end
        end
      end
    end
  end
end
