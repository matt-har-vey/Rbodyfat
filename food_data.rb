require 'json'
require 'sqlite3'
require 'httparty'
require 'pp'
require 'date'

class FoodData
	Username = ENV['FITBIT_USERNAME']
	Password = ENV['FITBIT_PASSWORD']
	ReportDays = 7
	Database = 'weights.sqlite3'

	include HTTParty
	base_uri 'https://www.myfitnesspal.com'
	headers 'Accept' => 'application/json, text/json'

	def initialize
		if Username.nil? || Password.nil?
			raise 'environment variables FITBIT_USERNAME and FITBIT_PASSWORD are required'
		end
		params = { :username => Username, :password => Password }
		response = self.class.post('/account/login', :body => params)
		@cookie = response.request.options[:headers]['Cookie']
		self.class.base_uri 'http://www.myfitnesspal.com'
	end

	def table
		@table || load
	end

	def persist
		t = table
		db = SQLite3::Database.new(Database)
		db.transaction do |d|
			t.each do |date, vals|
				d.execute("DELETE FROM food_intakes WHERE date=?", date.to_s)
				d.execute("INSERT INTO food_intakes (date, calories, net_calories, carbs, fat, protein, fiber, sugar, sodium, potassium) VALUES (?,?,?,?,?,?,?,?,?,?)",
                   date.to_s, vals[:calories], vals[:net_calories], vals[:carbs], vals[:fat], vals[:protein], vals[:fiber], vals[:sugar], vals[:sodium], vals[:potassium])
			end
		end
	end

	private
	  def report(kind)
	  	response = self.class.get("/reports/results/nutrition/#{kind}/#{ReportDays}.json?report_name=#{kind}",
	  							:headers => { 'Cookie' => @cookie })

	  	vals = {}
	  	JSON.parse(response.body)['data'].map do |o|
	  		vals[parse_date(o['date'])] = o['total']
	  	end
	  	vals
	  end

	  def parse_date(s)
	  	month, day = s.split('/').map(&:to_i)
	  	year = Date.today.year
	  	date = Date.new(year, month, day)
	  	if date > Date.today
	  		Date.new(year - 1, month, day)
	  	else
	  		date
	  	end
	  end

	  def load
			@table = {}

	  	kind_column = [['Calories', :calories], ['Net%20Calories', :net_calories], ['Carbs', :carbs],
	     ['Fat', :fat], ['Protein', :protein], ['Fiber', :fiber], ['Sugar', :sugar],
	  	 ['Sodium', :sodium], ['Potassium', :potassium]]

			kind_column.each do |rk|
	  		kind = rk.first
	  		sym = rk.last
        vals = report(kind)
	  		vals.each do |k,v|
	  			@table[k] ||= {}
	  			@table[k][sym] = v
	  		end
	  	end

			@table.keep_if do |day, vals|
				vals[:calories] && vals[:calories] > 0
			end

	  	@table
	  end
end

fd = FoodData.new
fd.persist
