from dataclasses import dataclass


@dataclass
class Carro:
    """ Interp.: Um carro na posicao (x, y) da tela com combustivel restante no tanque """
    x: int
    y: int
    combustivel: int


    """
    def fn_para_carro(c: Carro) -> Carro:
        return c
    """


EX_CARRO_1 = Carro(100, 100, 100)
EX_CARRO_2 = Carro(200, 100, 0)


def sem_combustivel(c: Carro) -> bool:
    """ Produz True se o carro 'c' esta sem combustivel, ou seja, se c.combustivel eh zero """
    return c.combustivel == 0


assert sem_combustivel(EX_CARRO_1) == False
assert sem_combustivel(EX_CARRO_2) == True
