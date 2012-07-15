#!/usr/bin/env ruby

require "rubygems"
require "time"
require "json"

maph = {}
aih = {}
results = Hash.new {|h,k| h[k] = {} }
Dir.glob('*.result').each do |file|
  basename = file.gsub(/\.result$/, '')
  json = JSON.parse(File.read(file))
  # ai, map, *c = File.readlines(file).first.chomp.split(' ')
  # {:ai => ai, :map => map, :time => c.join(' ') }
  map = json["map"]
  ai = json["ai"]
  maph[map] = true
  aih[ai] = true

  json["basename"] = basename
  results[map][ai] = json
  # { :basename => basename, :elapsed => c.join(' ').to_i }

  lastl = File.readlines(basename + '.out')[-1]
  if lastl =~ /^(Win|Dead|Abort) \(?([-0-9]+)\)?$/
    results[map][ai]["status"] = $1
    results[map][ai]["score"] = $2
  end
end

ais = aih.keys.sort
maps = maph.keys.sort

output = ['<!DOCTYPE html><html lang="ja"><head><title>MATCH LIST</title>',
          '<link rel="stylesheet" href="default.css" type="text/css" />',
          '</head>',
          "<body><h1>MATCH LIST</h1>",
          "<table>",
          '<tr><th>MAP</th><th>' + ais.map{|n| n.split('/')[-1] }.join('</th><th>') + '</th></tr>']

maps.each do |map|
  mname = File.basename(map)
  airesults = results[map]
  output << "<tr><th>#{map}</th>"
  ais.each do |ai|
    aname = ai.split('/')[-1]
    r = airesults[ai]
    if r
      if r["succeed"]
        status = r["status"]
        txt = "#{r["status"]} #{r["score"]}"
      else
        status = "LTE"
        txt = "LTE"
      end
      output << "<td class=\"ret-#{status}\"><a href=\"#{r["basename"]}.out\">#{txt}</a> [<a href=\"#{r["basename"]}.err\">E</a>] [#{r["elapsed"]}]</td>"
    end
  end
  output << "</tr>"
end

puts output.join
