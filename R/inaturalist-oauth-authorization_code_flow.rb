require 'rubygems'
require 'rest_client'
require 'json'
 
site = "https://www.inaturalist.org"
app_id = 'YOUR APP ID'
app_secret = 'YOUR APP SECRET'
redirect_uri = 'YOUR APP REDIRECT URI' # you can set this to some URL you control for testing
 
# REQUEST AN AUTHORIZATION CODE
# Your web app should redirect the user to this url. They should see a screen
# offering them the choice to authorize your app. If they aggree, they will be
# redirected to your redirect_uri with a "code" parameter
url = "#{site}/oauth/authorize?client_id=#{app_id}&redirect_uri=#{redirect_uri}&response_type=code"
 
# REQUEST AN AUTH TOKEN
# Once your app has that code parameter, you can exchange it for an access token:
puts "Go to #{url}, approve the app, and you should be redirected to your " + 
  "redirect_uri. Copy and paste the 'code' param here."
print "Code: "
auth_code = gets.strip
puts

payload = {
  :client_id => app_id,
  :client_secret => app_secret,
  :code => auth_code,
  :redirect_uri => redirect_uri,
  :grant_type => "authorization_code"
}
puts "POST #{site}/oauth/token, payload: #{payload.inspect}"
puts response = RestClient.post("#{site}/oauth/token", payload)
puts
# response will be a chunk of JSON looking like
# {
#   "access_token":"xxx",
#   "token_type":"bearer",
#   "expires_in":null,
#   "refresh_token":null,
#   "scope":"write"
# }
 
# Store the token (access_token) in your web app. You can now use it to make authorized
# requests on behalf of the user, like retrieving profile data:
token = JSON.parse(response)["access_token"]
headers = {"Authorization" => "Bearer #{token}"}
puts "GET /users/edit.json, headers: #{headers.inspect}"
puts RestClient.get("#{site}/users/edit.json", headers)
puts