<script setup>
import router from '@/router';
import apiService from '@/services/api-service';
import { useCorrentistaStore } from '@/stores/CorrentistaStore';
import debounce from 'lodash.debounce'
import { watch, defineEmits } from 'vue';
const emit = defineEmits(['showError'])
const numConta = ref('')
const valor = ref('0')
const nome = ref('')
const contaDestinoId = ref(0)
const form = ref(false)
const correntistaStore = useCorrentistaStore()

watch(numConta, debounce((newCode) => {
  try {
    apiService.carregarContaCorrentePorNumConta(newCode)
      .then(({data}) => {
        const mainAccount = data[0]
        if(!mainAccount) {
          nome.value = ''
          contaDestinoId.value = 0
          valor.value = '0'
          throw new Error('Conta não encontrada.');
        }

        
        if(mainAccount.id === correntistaStore.contaCorrentePrincipalId)
        throw new Error('Não é permitido realizar transferências para si mesmo.')
        
        contaDestinoId.value = mainAccount.id
        return apiService.carregarCorrentista(mainAccount.correntistaId)
      })
      .then(({data}) => nome.value = data.nome)
      .catch(x => console.error('Erro ao carregar conta corrente', x))

      console.log(numConta, contaDestinoId, nome)
  } catch (error) {
    vencimento.value = null
    valor.value = ''
    console.error('Erro! Não foi possível ler o código de barras. ' + error)
  }
}, 500))
function transferir() {
  if (!form) return

  const floatValue = getFloatValue(valor.value)
  apiService.transferir(correntistaStore.contaCorrentePrincipalId,contaDestinoId.value, floatValue)
    .then(({data}) => {
      if(data.result === 'failure')
        return emit('showError', data.error)
      else
        return router.push('/dashboard')
    })
    .catch(e => emit('showError', {code: 1, message: e.message}))
}

function getFloatValue(v) {
  if(!v) return 0
  return parseFloat(v.replace(/\./g, '').replace(',', '.'))
}

function valorRequired (v) {
  return getFloatValue(v) > 0 || 'Digite um valor válido'
}

async function contaValida(v) {
  if(!contaDestinoId.value)
    return 'Beneficiário desconhecido.'
  return v
}

</script>

<template>
  <VForm v-model="form" @submit.prevent="transferir">
    <VRow>
      <VCol cols="12">
        <VTextField
          type="text"
          v-model="numConta"
          label="Número conta beneficiário"
          v-mask="'000000-0'"
          placeholder="000000-0"
        />
      </VCol>
      <VCol cols="6" v-show="contaDestinoId">
        <VTextField
          v-model="nome"
          label="Beneficiário"
          readonly
        />
      </VCol>
      <VCol cols="6" v-show="contaDestinoId">
        <VTextField
          :rules="[valorRequired]"
          type="text"
          v-mask-decimal.br="2"
          v-model="valor"
          label="Valor"
          placeholder="0,00"
        />
      </VCol>
      <VCol
      cols="12"
      class="d-flex gap-4"
      >
        <VBtn type="submit" :disabled="!form">
          Pagar 🚀
        </VBtn>

      </VCol>
    </VRow>
  </VForm>
</template>
