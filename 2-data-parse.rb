# Originally written by Hadley Wickham, copied and modified from https://github.com/hadley/data-baby-names/ --------

require 'nokogiri'
require 'csv'

def parse_year(year)
    doc = Nokogiri::HTML(open("original/#{year}.html"))
	rows = (doc/"body/table[3]/tr/td[2]/table/tr")

    CSV.open("raw/#{year}.csv", "w") do |csv|
      rows.each {|row| csv << parse_row(row)}
    end
end

def parse_row(row)
    (row/"td").map{|e| e.content}
end

(1880..2016).each{|year| parse_year(year)}
