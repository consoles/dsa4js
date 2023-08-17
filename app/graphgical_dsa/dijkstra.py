# dijsktra 求有向加权图的最小路径
# https://git-hexo-blog.oss-cn-beijing.aliyuncs.com/dijkstra_min_plane_price.png

class City:
    def __init__(self, name):
        self.name = name
        # 把表示邻接点的数组转换成散列表
        # 如果此城市是 Atlanta，则散列表包含
        # { boston: 100, denver: 160 }
        self.routes = {}

    def add_route(self, city, price):
        self.routes[city] = price

atlanta = City("Atlanta")
boston = City("Boston")
chicago = City("Chicago")
denver = City("Denver")
el_paso = City("El Paso")

atlanta.add_route(boston, 100)
atlanta.add_route(denver, 160)

boston.add_route(chicago, 120)
boston.add_route(denver, 180)

chicago.add_route(el_paso, 80)

denver.add_route(chicago, 40)
denver.add_route(el_paso, 140)

# 找出最便宜的航线
def dijkstra(start_city, other_cities):
    # 保存从给定城市到其他所有城市的价格以及途径的城市
    routes_from_city = {}
    # { 终点城市: [价格, 到达终点城市前所需要经过的那个城市] }，举例如下：
    # { atlanta: [0, None], boston: [100, altanta], chicago: [120, boston], denver: [160, altanta], el_paso: [280, chicago] }

    # 从起点到起点城市是免费的
    routes_from_city[start_city] = [0, start_city]

    # 初始化该散列表的时候，因为去往其他所有城市的花费都是未知的，所以先设置为无限
    for city in other_cities:
        routes_from_city[city] = [float('inf'), None]

    # 记录已经访问的城市
    visited_cities = []

    # 一开始访问起点城市
    current_city = start_city

    # 循环访问每个城市
    while current_city:
        visited_cities.append(current_city)
        for city, price in current_city.routes.items():
            # 如果起点城市到其他城市的价格比 routes_from_city 所记录的低，则更新记录
            if routes_from_city[city][0] > price + routes_from_city[current_city][0]:
                routes_from_city[city] = [price + routes_from_city[current_city][0], current_city]

        # 决定下一个要访问的城市
        current_city = None
        cheapest_route_from_current_city = float('inf')
        # 记录所有已经记录的路线
        for city, price in routes_from_city.items():
            if price[0] < cheapest_route_from_current_city and city not in visited_cities:
                current_city = city
                cheapest_route_from_current_city = price[0]
    return routes_from_city            

# 调用
routes = dijkstra(atlanta, [boston, chicago, denver, el_paso])
for city, price in routes.items():
    print(city.name, price[0])
