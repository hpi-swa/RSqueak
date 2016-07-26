require "net/http"
require "pp"
require "travis/client"
require "active_support"
require "active_support/core_ext/numeric"
require "active_support/core_ext/date"
require "active_support/core_ext/time"

CODESPEED = URI("http://lively-kernel.org/codespeed")

client = Travis::Client.new(access_token: ENV["TRAVIS_API_TOKEN"])
repo = client.repo(ENV["REPOSITORY"])
vm = ENV["VM"]

repo.builds.select do |b|
  break if (b.finished_at && b.finished_at < 1.day.ago)
  b.passed?
end.group_by do |b|
  b.commit.branch
end.each_pair do |branch, builds|
  last_build = builds.first
  commitdate = last_build.commit.committed_at
  commitver = ("%4s%2s%2s%2s%2s" % [commitdate.year, commitdate.month, commitdate.day,
                                    commitdate.hour, commitdate.minute]).gsub(" ", "0")
  Net::HTTP.start(CODESPEED.host, CODESPEED.port) do |http|
    req = Net::HTTP::Post.new(uri.path)
    req["commitid"] = (ENV["USE_SHA"] ? last_build.commit.sha : commitver)
    req["branch"] = branch
    req["vm"] = vm
    pp req
    http.request(req)
    req["vm"] = "#{vm}64"
    pp req
    http.request(req)
  end
end
