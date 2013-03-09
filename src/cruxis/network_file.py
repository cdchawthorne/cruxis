class NetworkFile:
    def __init__(self, filename):
        self.__filename = filename

    @property
    def remembered_ids(self):
        with open(self.__filename, "r") as f:
            return [int(line.rstrip()) for line in f]

    def remember_network(self, network_id):
        network_id = str(network_id)
        with open(self.__filename, "r") as f:
            network_ids = [line.rstrip() for line in f]

        if network_id not in network_ids:
            with open(self.__filename, "a") as f:
                f.write(network_id + "\n")

    def forget_network(self, network_id):
        with open(self.__filename, "r") as f:
            remembered_ids = [line.rstrip() for line in f]

        with open(self.__filename, "w") as f:
            for remembered_id in remembered_ids:
                if remembered_id != str(network_id):
                    f.write(remembered_id + "\n")
