# encoding: UTF-8

# the darnedest thing
# ▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬

# ............................................................. Syndication feed

module Rss
  def self.xml
    LOG.info('Building "rss" xml page')
    # assemble date insert
    wikis = WikiList.new
    feed_count = 0
    date = nil
    xml = %[<?xml version="1.0" encoding="utf-8"?>
      <rss version="2.0">
      <channel>
      <title>#{HOMEPAGE}</title>
      <description>#{META_DESCRIPTION}</description>
      <lastBuildDate>#{Time.now.strftime('%a, %d %b %Y %H:%M:%S %Z')}</lastBuildDate>
      <language>en-us</language>
      <link>#{TDT_URI}</link>
    ]
    wikis.directory.each do |filename|
      uri = File.uri(filename)
      wiki = Wiki.new(uri)
      unless wiki.empty?
        unless wiki.publishing[:date] == date
          break if feed_count >= RSS_CUTOFF
          date = wiki.publishing[:date]
        end
        xml << "<item>"
        xml << "<title>#{wiki.title}</title>"
        xml << "<description>#{wiki.plain_text(RSS_WORDS)} ...</description>"
        xml << "<link>#{TDT_URI}/#{wiki.encode_uri}</link>"
        xml << "<guid>#{TDT_URI}/#{wiki.encode_uri}</guid>"
        xml << "<pubDate>#{File.stat(filename).mtime.strftime('%a, %d %b %Y %H:%M:%S %Z')}</pubDate>"
        xml << "</item>"
        feed_count += 1
      end
    end
    xml << '
      </channel>
      </rss>
    '
  end
end

