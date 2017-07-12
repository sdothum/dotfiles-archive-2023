require "yaml"
ignore, contacts = YAML::load_file( ENV['HOME'] + "/.bitsa_cache.yml" )

contacts.values.flatten.each_slice(2).map do |e,n|
  "#{e}\t#{n}"
end
