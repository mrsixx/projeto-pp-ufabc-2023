<script setup>
import apiService from '@/services/api-service';
import { useCorrentistaStore } from '@/stores/CorrentistaStore';
import { watch, defineEmits } from 'vue';
const code = ref('')
const emit = defineEmits(['showError'])
const vencimento = ref('')
const valor = ref('0')
const form = ref(false)
import { validarBoleto } from '@mrmgomes/boleto-utils'
import router from '@/router';
const correntistaStore = useCorrentistaStore()

watch(code, (newCode) => {
  try {
    const boleto = validarBoleto(newCode, 'LINHA_DIGITAVEL')
    console.log(boleto)
    const {vencimento: vencimentoBoleto, valor: valorBoleto} = boleto
      vencimento.value = vencimentoBoleto.toISOString().split('T')[0]
      valor.value = valorBoleto.toLocaleString('pt-BR')
  } catch (error) {
    vencimento.value = null
    valor.value = ''
    console.error('Erro! Não foi possível ler o código de barras. ' + error)
  }
})
function pagar() {
  if (!form) return

  const floatValue = getFloatValue(valor.value)
  apiService.pagar(correntistaStore.contaCorrentePrincipalId, floatValue)
    .then(({data}) => {
      if(data.result === 'failure')
        emit('showError', data.error)
      else
        router.push('/dashboard')
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

</script>

<template>
  <VForm v-model="form" @submit.prevent="pagar">
    <VRow>
      <VCol cols="12">
        <VTextField
          type="text"
          v-model="code"
          label="Linha Digitável"
          placeholder="000000000000 000000000000 000000000000 000000000000"
        />
      </VCol>
      <VCol cols="6">
        <VTextField
          :rules="[valorRequired]"
          type="text"
          v-mask-decimal.br="2"
          v-model="valor"
          label="Valor"
          placeholder="0,00"
        />
      </VCol>
      <VCol cols="6">
        <VTextField
          :rules="[required]"
          type="date"
          v-model="vencimento"
          label="Vencimento"
          readonly
          placeholder="dd/mm/yyyy"
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
