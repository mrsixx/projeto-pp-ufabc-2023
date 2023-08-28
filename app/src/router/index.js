import { createRouter, createWebHistory } from 'vue-router'

const router = createRouter({
  history: createWebHistory(import.meta.env.BASE_URL),
  routes: [
    { path: '/', redirect: '/login' },
    {
      path: '/',
      component: () => import('../layouts/default.vue'),
      children: [
        {
          path: 'dashboard',
          component: () => import('../pages/dashboard.vue'),
        },
        {
          path: 'deposito',
          component: () => import('../pages/deposito.vue'),
        },
        {
          path: 'pagamento',
          component: () => import('../pages/pagamento.vue'),
        },
        {
          path: 'transferencia',
          component: () => import('../pages/transferencia.vue'),
        },
        {
          path: 'extrato',
          component: () => import('../pages/extrato.vue'),
        },
        {
          path: 'account-settings',
          component: () => import('../pages/account-settings.vue'),
        },
        {
          path: 'typography',
          component: () => import('../pages/typography.vue'),
        },
        {
          path: 'icons',
          component: () => import('../pages/icons.vue'),
        },
        {
          path: 'cards',
          component: () => import('../pages/cards.vue'),
        },
        {
          path: 'tables',
          component: () => import('../pages/tables.vue'),
        },
        {
          path: 'form-layouts',
          component: () => import('../pages/form-layouts.vue'),
        },
      ],
    },
    {
      path: '/',
      component: () => import('../layouts/blank.vue'),
      children: [
        {
          path: 'login',
          component: () => import('../pages/login.vue'),
        },
        {
          path: '/:pathMatch(.*)*',
          component: () => import('../pages/[...all].vue'),
        },
      ],
    },
  ],
})

// garante que apenas correntistas logados terão acesso à dashboard
router.beforeEach(async (to, _, next) => {
  const loggedIn = localStorage.getItem('pp-correntista-id')
  const allowed = loggedIn || to.path === '/login';
  if (allowed) next();
  else next('/');
});
export default router
