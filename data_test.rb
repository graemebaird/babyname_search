require 'nokogiri'
require 'csv'

def parse_row(row)
    (row/"td").map{|e| e.content}
end

doc = Nokogiri::HTML(open("original/1880.html"))
rows = (doc/"body/table[3]/tr/td[2]/table/tr")
CSV.open("raw/1880.csv", "w") do |csv|
  rows.each {|row| csv << parse_row(row)}
 end
