#!/usr/bin/env ruby

require "fileutils"
require "time"

aipath = ARGV.shift
newname = []
File.readlines(aipath).map(&:chomp).each do |prog|
  mt = File.mtime(prog)
  dir = File.dirname(prog)
  f = File.basename(prog)
  fn="#{mt.to_i}-#{f.gsub(%r"ll-ai-", '')}"
  newprog = File.join(dir, fn)
  FileUtils.cp(prog, newprog)
  File.utime(mt, mt, newprog)
  newname.push(newprog)
end

open(aipath, 'w') {|out| out.puts newname }
