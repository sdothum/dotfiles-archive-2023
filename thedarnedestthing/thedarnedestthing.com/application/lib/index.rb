# encoding: UTF-8

# the darnedest thing
# ▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬

include XapianFu

module Index
  class << self

    # .................................................... Build search database

    def update
      LOG.info('Building search database')
      FileUtils.rm_rf('thedarnedestthing.db')
      db = XapianDb.new(:dir => 'thedarnedestthing.db', :create => true, :store => [:uri, :date, :title, :text])
      wikis = WikiList.new
      wikis.directory.each do |filename|
        uri = File.uri(filename)
        wiki = Wiki.new(uri)
        unless wiki.empty?
          db << { :uri => uri, :date => File.stat(filename).mtime.to_s, :title => wiki.title, :text => wiki.words }
          LOG.debug("Wiki => #{wiki.title} Words => #{wiki.words}")
        else
          LOG.info(%["#{uri}" from "#{wiki.homepage}" is empty.. delete from db])
        end
      end
      db.flush
      LOG.info('Building search database complete')
    end

    # ......................................................... Full text search

    def search(keywords)
      LOG.info(%[Searching database for "#{keywords}"])
      db = XapianDb.new(:dir => 'thedarnedestthing.db')
      content = '<div id="results">'
      history = Footer.new('history')
      day_count = 0
      date = nil
      keywords.rstrip!
      db.search(keywords, :order => :date).reverse.each do |match|
        uri = match.values[:uri]
        wiki = Wiki.new(uri)
        unless wiki.empty? or HideArticle.call(wiki.homepage)
          unless wiki.publishing[:date] == date
            if day_count < SEARCH_CUTOFF
              date = wiki.publishing[:date]
              content << %[<h2><a href="/date##{wiki.publishing[:month]}?year=#{wiki.publishing[:year]}">#{date}</a></h2>]
            else
              history << wiki.href
              next
            end
            day_count += 1
          end
          content << Content.snippet(wiki, :show_comment)
        end
      end
      if day_count == 0
        content << "<p>no articles found. "
        if keywords.split.size == 1
          content << "Perhaps try searching for:&nbsp;&nbsp;<strong>#{keywords}*</strong>"
        elsif keywords.split.size > 1
          content << "Perhaps try searching for:&nbsp;&nbsp;<strong>#{keywords.gsub(/ +/, ' or ')}</strong>"
          content << "&nbsp;&nbsp;or&nbsp;&nbsp;<strong>#{keywords.gsub(/ +/, '* ') + '*'}</strong>"
        end
        content << %[</p><p class="indent">See <a class="article" href="/searching this site">searching this site</a> for a more detailed explanation on how to construct and refine queries..</p>]
      end
      content << history.links
      content << "</div>"
    end
  end
end
