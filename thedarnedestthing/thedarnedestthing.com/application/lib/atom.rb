# encoding: UTF-8

# the darnedest thing
# ▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬▬

# ............................................................. Syndication feed

module Atom
  def self.xml
    LOG.info('Building "atom" xml page')
    # assemble date insert
    wikis = WikiList.new
    feed_count = 0
    date = nil
    xml = %[<feed xmlns="http://www.w3.org/2005/Atom">
      <title>#{HOMEPAGE}</title>
      <subtitle>#{META_DESCRIPTION}</subtitle>
      <link rel="alternate" type="text/html" href="#{TDT_URI}/"/>
      <link rel="self" type="application/atom+xml" href="#{TDT_URI}/atom.xml"/>
      <id>#{TDT_URI}/atom.xml</id>
      <updated>#{Time.now.iso8601}</updated>
      <rights>Copyright © 2013, Steven Hum</rights>
    ]
    wikis.directory.each do |filename|
      uri = File.uri(filename)
      wiki = Wiki.new(uri)
      unless wiki.empty?
        unless wiki.publishing[:date] == date
          break if feed_count >= RSS_CUTOFF
        end
        xml << "<entry>"
        xml << "<title>#{wiki.title}</title>"
        xml << %[<link rel="alternate" type="text/html" href="#{TDT_URI}/#{wiki.encode_uri}"/>]
        xml << %[<id>#{TDT_URI}/#{wiki.encode_uri}</id>]
        xml << %[<published>#{File.stat("#{wiki.filename}").ctime.iso8601}</published>]
        xml << %[<updated>#{File.stat("#{wiki.filename}").mtime.iso8601}</updated>]
        xml << "<author>"
        xml << "<name>Steven Hum</name>"
        xml << "<uri>#{TDT_URI}/</uri>"
        xml << "</author>"
        xml << %[<content type="html">]
        xml << "<![CDATA["
        xml << "#{wiki.content}"
        xml << "]]>"
        xml << "</content>"
        xml << "</entry>"
        feed_count += 1
      end
    end
    xml << '
      </feed>
    '
  end
end

