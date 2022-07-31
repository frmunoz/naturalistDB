require "rubygems"
require "rest_client"
require "digest"
require "base64"

site = "https://www.inaturalist.org"
app_id = 'YOUR_APP_ID'
redirect_uri = 'YOUR_REDIRECT_URL' # you can set this to some URL you control for testing
code_verifier = "supersecretverifier"

# Encode your code_verifier
# For notes on the modified Base64 encoding, see https://tools.ietf.org/html/rfc7636#appendix-A
# Note that Ruby's Base64#urlsafe_encode64 does *not* seem to work
code_challenge = Base64.encode64(Digest::SHA256.digest(code_verifier))
code_challenge = code_challenge.split("=")[0]
code_challenge.gsub!("+", "-")
code_challenge.gsub!("/", "_")
 
# REQUEST AN AUTHORIZATION CODE
# Your web app should redirect the user to this url. They should see a screen
# offering them the choice to authorize your app. If they aggree, they will be
# redirected to your redirect_uri with a "code" parameter
url = "#{site}/oauth/authorize?client_id=#{app_id}&redirect_uri=#{redirect_uri}&response_type=code&code_challenge_method=S256&code_challenge=#{code_challenge}"
 
# REQUEST AN AUTH TOKEN
# Once your app has that code parameter, you can exchange it for an access token:
puts "Go to #{url}, approve the app, and you should be redirected to your " + 
  "redirect_uri. Copy and paste the 'code' param here."
print "Code: "
auth_code = gets.strip
puts

payload = {
  :client_id => app_id,
  :code => auth_code,
  :redirect_uri => redirect_uri,
  :grant_type => "authorization_code",
  :code_verifier => code_verifier
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
# requests on behalf of the user, like retrieving a JSON Web Token:
token = JSON.parse(response)["access_token"]
headers = {"Authorization" => "Bearer #{token}"}
puts "GET /users/api_token"
puts RestClient.get("#{site}/users/api_token", headers)
puts